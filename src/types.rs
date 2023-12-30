use core::fmt;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Neg, Sub};
use std::str::FromStr;

use thiserror::Error;
use winsafe::{co, prelude::NativeBitflag, GmidxEnum, DISPLAY_DEVICE, POINT};
use serde;
use serde::ser::{Serialize, SerializeStruct, Serializer};
use serde::de::{self, Deserialize, Deserializer, Visitor, SeqAccess, MapAccess};

/// Error type for the display module
#[derive(Error, Debug)]
pub enum DisplayPropertiesError {
    #[error("Display {0} has no settings")]
    NoSettings(String),
    #[error("Error when calling the Windows API")]
    WinAPI(#[from] co::ERROR),
    #[error("Apply failed, returned flags: {0}")]
    ApplyFailed(co::DISP_CHANGE),
    #[error("Invalid orientation: {0}")]
    InvalidOrientation(String),
    #[error("Invalid fixed output: {0}")]
    InvalidFixedOutput(String),
}

type DisplayPropertyResult<T = ()> = std::result::Result<T, DisplayPropertiesError>;

/// Contains the properties of a display
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct DisplayProperties {
    pub name: String,

    pub string: String,
    pub key: String,

    pub active: bool,
    pub primary: bool,

    pub settings: Option<RefCell<DisplaySettings>>,
}

impl fmt::Display for DisplayProperties {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Display {{ key: {}, name: {}, string: {}, active: {}, primary: {} }}",
            self.key, self.name, self.string, self.active, self.primary
        )
    }
}

/// Contains the settings of a display
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DisplaySettings {
    pub position: Position,
    pub resolution: Resolution,
    pub orientation: Orientation,
    pub fixed_output: FixedOutput,
    pub refresh_rate: RefreshRate,
}

impl DisplayProperties {
    /// Create a display properties struct from a winsafe display
    pub fn from_winsafe(device: &DISPLAY_DEVICE) -> DisplayPropertyResult<DisplayProperties> {
        let active = device.StateFlags.has(co::DISPLAY_DEVICE::ATTACHED_TO_DESKTOP);
        let settings = if active {
            Some(RefCell::new(Self::fetch_settings(&device.DeviceName())?))
        } else {
            None
        };

        Ok(DisplayProperties {
            name: device.DeviceName(),
            string: device.DeviceString(),
            key: device.DeviceKey(),
            active,
            primary: device.StateFlags.has(co::DISPLAY_DEVICE::PRIMARY_DEVICE),
            settings,
        })
    }

    /// Fetch the settings of a display
    fn fetch_settings(name: &str) -> DisplayPropertyResult<DisplaySettings> {
        let mut devmode = winsafe::DEVMODE::default();
        winsafe::EnumDisplaySettings(
            Some(name),
            GmidxEnum::Enum(winsafe::co::ENUM_SETTINGS::CURRENT),
            &mut devmode,
        )?;

        Ok(DisplaySettings {
            position: Position(devmode.dmPosition()),
            resolution: Resolution::new(devmode.dmPelsWidth, devmode.dmPelsHeight),
            orientation: Orientation::from_winsafe(devmode.dmDisplayOrientation())?,
            fixed_output: FixedOutput::from_winsafe(devmode.dmDisplayFixedOutput())?,
            refresh_rate: RefreshRate(devmode.dmDisplayFrequency)
        })
    }

    /// Apply the settings of the display
    pub fn apply(&self) -> DisplayPropertyResult {
        if self.settings.is_none() {
            return Err(DisplayPropertiesError::NoSettings(self.name.to_string()));
        }
        let settings = self.settings.as_ref().unwrap().borrow(); // safe, because we just checked it

        let mut flags =
            winsafe::co::CDS::UPDATEREGISTRY | winsafe::co::CDS::NORESET | winsafe::co::CDS::GLOBAL;

        if self.primary {
            flags |= winsafe::co::CDS::SET_PRIMARY;
        }

        let mut devmode = winsafe::DEVMODE::from_display_settings(
            settings.position,
            settings.orientation,
            settings.fixed_output,
            settings.resolution,
            settings.refresh_rate
        );

        let result = winsafe::ChangeDisplaySettingsEx(Some(&self.name), Some(&mut devmode), flags);
        // use into_ok_or_err as soon it is stable
        match result {
            Ok(_) => Ok(()),
            Err(err) => Err(DisplayPropertiesError::ApplyFailed(err)),
        }
    }
}

/// Provides methods to set properties of `winsafe::DEVMODE`
trait FromDisplaySettings {
    fn set_position(&mut self, position: Position);
    fn set_orientation(&mut self, orientation: Orientation);
    fn set_fixed_output(&mut self, fixed_output: FixedOutput);
    fn set_resolution(&mut self, resolution: Resolution);
    fn set_refresh_rate(&mut self, refresh_rate: RefreshRate);

    /// Converts display settings into a `winsafe::DEVMODE` struct
    fn from_display_settings(
        position: Position,
        orientation: Orientation,
        fixed_output: FixedOutput,
        resolution: Resolution,
        refresh_rate: RefreshRate
    ) -> winsafe::DEVMODE {
        let mut devmode = winsafe::DEVMODE::default();
        devmode.set_position(position);
        devmode.set_orientation(orientation);
        devmode.set_fixed_output(fixed_output);
        devmode.set_resolution(resolution);
        devmode.set_refresh_rate(refresh_rate);
        devmode
    }
}

impl FromDisplaySettings for winsafe::DEVMODE {
    fn set_position(&mut self, position: Position) {
        self.set_dmPosition(position.0);
        self.dmFields |= winsafe::co::DM::POSITION;
    }

    fn set_orientation(&mut self, orientation: Orientation) {
        self.set_dmDisplayOrientation(orientation.to_winsafe());
        self.dmFields |= winsafe::co::DM::DISPLAYORIENTATION;
    }

    fn set_fixed_output(&mut self, fixed_output: FixedOutput) {
        self.set_dmDisplayFixedOutput(fixed_output.to_winsafe());
        self.dmFields |= winsafe::co::DM::DISPLAYFIXEDOUTPUT;
    }

    fn set_resolution(&mut self, resolution: Resolution) {
        self.dmPelsWidth = resolution.width;
        self.dmPelsHeight = resolution.height;
        self.dmFields |= winsafe::co::DM::PELSWIDTH | winsafe::co::DM::PELSHEIGHT;
    }

    fn set_refresh_rate(&mut self, refresh_rate: RefreshRate) {
        self.dmDisplayFrequency = refresh_rate.0;
        self.dmFields |= winsafe::co::DM::DISPLAYFREQUENCY;
    }
}

/// Contains the position of a display
#[derive(Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Position(POINT);

impl Position {
    /// Create a position
    pub fn new(x: i32, y: i32) -> Self {
        Self(POINT { x, y })
    }
}

impl Serialize for Position {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer {
        let mut state = serializer.serialize_struct("Position", 2)?;
        state.serialize_field("x", &self.0.x)?;
        state.serialize_field("y", &self.0.y)?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for Position {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
    {

       #[derive(serde::Deserialize)]
       #[serde(field_identifier, rename_all = "lowercase")]
        enum Field { X, Y }
        
        struct PositionVisitor;

        impl<'de> Visitor<'de> for PositionVisitor {
            type Value = Position;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Position")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Position, V::Error>
                where
                    V: SeqAccess<'de>,
            {
                let x = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let y = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &self))?;
                Ok(Position::new(x, y))
            }

            fn visit_map<V>(self, mut map: V) -> Result<Position, V::Error>
                where
                    V: MapAccess<'de>,
            {
                let mut x = None;
                let mut y = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::X => {
                            if x.is_some() {
                                return Err(de::Error::duplicate_field("x"));
                            }
                            x = Some(map.next_value()?);
                        }
                        Field::Y => {
                            if y.is_some() {
                                return Err(de::Error::duplicate_field("y"));
                            }
                            y = Some(map.next_value()?);
                        }
                    }
                }
                let x_value = x.ok_or_else(|| de::Error::missing_field("x"))?;
                let y_value = y.ok_or_else(|| de::Error::missing_field("y"))?;
                Ok(Position::new(x_value, y_value))
            }
        }

        const FIELDS: &'static [&'static str] = &["x", "y"];
        deserializer.deserialize_struct("Position", FIELDS, PositionVisitor)
    }
}

impl Add for Position {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(POINT {
            x: self.0.x + other.0.x,
            y: self.0.y + other.0.y,
        })
    }
}

impl Sub for Position {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self(POINT {
            x: self.0.x - other.0.x,
            y: self.0.y - other.0.y,
        })
    }
}

impl Neg for Position {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(POINT {
            x: -self.0.x,
            y: -self.0.y,
        })
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.0.x, self.0.y)
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Point")
            .field(&self.0.x)
            .field(&self.0.y)
            .finish()
    }
}

/// Errors that occur while parsing a position from a string
#[derive(Error, Debug)]
pub enum ParsePositionError {
    #[error("Error parsing integer")]
    IntError(#[from] std::num::ParseIntError),
    #[error("First part missing")]
    FirstPart,
    #[error("Second part missing. Expected format: <x>,<y>")]
    SecondPart,
}

impl FromStr for Position {
    type Err = ParsePositionError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut parts = s.split(',');
        let x = parts.next().ok_or(ParsePositionError::FirstPart)?.parse()?;
        let y = parts
            .next()
            .ok_or(ParsePositionError::SecondPart)?
            .parse()?;
        Ok(Self::new(x, y))
    }
}

/// Contains the resolution of a display
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct Resolution {
    pub width: u32,
    pub height: u32,
}

impl Resolution {
    /// Creates a new resolution
    pub fn new(width: u32, height: u32) -> Self {
        Self { width, height }
    }
}

impl fmt::Display for Resolution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}x{}", self.width, self.height)
    }
}

/// Errors that occur while parsing a resolution from a string
#[derive(Error, Debug)]
pub enum ParseResolutionError {
    #[error("Error parsing integer")]
    IntError(#[from] std::num::ParseIntError),
    #[error("First integer missing")]
    FirstPart,
    #[error("Second integer missing. Expected format: <width>x<height>")]
    SecondPart,
}

impl FromStr for Resolution {
    type Err = ParsePositionError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut parts = s.split('x');
        let width = parts.next().ok_or(ParsePositionError::FirstPart)?.parse()?;
        let height = parts
            .next()
            .ok_or(ParsePositionError::SecondPart)?
            .parse()?;
        Ok(Self::new(width, height))
    }
}

/// Contains the orientation of a display
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum Orientation {
    Landscape,        // default
    LandscapeFlipped, // upside-down
    Portrait,         // rotate right
    PortraitFlipped,  // rotate left
}

impl Orientation {
    /// Creates a new orientation from `winsafe::co::DMD0`
    pub(crate) fn from_winsafe(co_dmdo: co::DMDO) -> DisplayPropertyResult<Self> {
        match co_dmdo {
            co::DMDO::DEFAULT => Ok(Orientation::Landscape),
            co::DMDO::D90 => Ok(Orientation::PortraitFlipped),
            co::DMDO::D180 => Ok(Orientation::LandscapeFlipped),
            co::DMDO::D270 => Ok(Orientation::Portrait),
            _ => Err(DisplayPropertiesError::InvalidOrientation(
                co_dmdo.to_string(),
            )),
        }
    }

    /// Creates the winsafe orientation struct
    fn to_winsafe(self) -> co::DMDO {
        match self {
            Orientation::Landscape => co::DMDO::DEFAULT,
            Orientation::PortraitFlipped => co::DMDO::D90,
            Orientation::LandscapeFlipped => co::DMDO::D180,
            Orientation::Portrait => co::DMDO::D270,
        }
    }
}

impl fmt::Display for Orientation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Orientation::Landscape => write!(f, "Default"),
            Orientation::LandscapeFlipped => write!(f, "UpsideDown"),
            Orientation::Portrait => write!(f, "Right"),
            Orientation::PortraitFlipped => write!(f, "Left"),
        }
    }
}

/// Errors that occur while parsing an orientation from a string
#[derive(Error, Debug)]
pub enum ParseOrientationError {
    #[error("Invalid orientation. Allowed values: `Default`, `UpsideDown`, `Right`, `Left`")]
    InvalidOrientation,
}

impl FromStr for Orientation {
    type Err = ParseOrientationError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "default" | "landscape" => Ok(Orientation::Landscape),
            "upsidedown" | "landscapeflipped" => Ok(Orientation::LandscapeFlipped),
            "right" | "portrait" => Ok(Orientation::Portrait),
            "left" | "portraitflipped" => Ok(Orientation::PortraitFlipped),
            _ => Err(ParseOrientationError::InvalidOrientation),
        }
    }
}

/// Contains the fixed output of a display
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum FixedOutput {
    Default,
    Stretch,
    Center,
}

impl FixedOutput {
    /// Creates a new fixed output struct from `winsafe::co::DMDF0`
    pub(crate) fn from_winsafe(co_dmdfo: co::DMDFO) -> DisplayPropertyResult<Self> {
        match co_dmdfo {
            co::DMDFO::DEFAULT => Ok(FixedOutput::Default),
            co::DMDFO::STRETCH => Ok(FixedOutput::Stretch),
            co::DMDFO::CENTER => Ok(FixedOutput::Center),
            _ => Err(DisplayPropertiesError::InvalidFixedOutput(
                co_dmdfo.to_string(),
            )),
        }
    }

    /// Creates a winsafe struct
    fn to_winsafe(self) -> co::DMDFO {
        match self {
            FixedOutput::Default => co::DMDFO::DEFAULT,
            FixedOutput::Stretch => co::DMDFO::STRETCH,
            FixedOutput::Center => co::DMDFO::CENTER,
        }
    }
}

impl fmt::Display for FixedOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FixedOutput::Default => write!(f, "Default"),
            FixedOutput::Stretch => write!(f, "Stretch"),
            FixedOutput::Center => write!(f, "Center"),
        }
    }
}

/// Errors that occur while parsing a fixed output from a string
#[derive(Error, Debug)]
pub enum ParseFixedOutputError {
    #[error("Invalid fxed output mode. Allowed values: `Default`, `Stretch`, `Center`")]
    InvalidFixedOutput,
}

impl FromStr for FixedOutput {
    type Err = ParseFixedOutputError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "default" => Ok(FixedOutput::Default),
            "stretch" => Ok(FixedOutput::Stretch),
            "center" => Ok(FixedOutput::Center),
            _ => Err(ParseFixedOutputError::InvalidFixedOutput),
        }
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct RefreshRate(pub u32);

/// Error type for the display module
#[derive(Error, Debug)]
pub enum DisplayStateError {
    #[error("Error in DisplayProperties")]
    Properties(#[from] DisplayPropertiesError),
    #[error("Error when calling the Windows API")]
    WinAPI(#[from] co::ERROR),
    #[error("Failed to commit the changes; Returned flags: {0}")]
    FailedToCommit(co::DISP_CHANGE),
}


/// A struct that represents a display (index)
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct DisplayState {
    pub device_key: String,
    pub position: Position,
    pub resolution: Resolution,
    pub refresh_rate: RefreshRate,
    pub fixed_output: FixedOutput,
    pub orientation: Orientation,
    pub is_primary: bool,
    pub is_enabled: bool,
}

impl DisplayState {
    pub fn new(windows_display_state: &DISPLAY_DEVICE) -> Option<DisplayState> {
        let device_name: String = windows_display_state.DeviceName();
        let mut dev_mode = winsafe::DEVMODE::default();

        if winsafe::EnumDisplaySettings(Some(device_name.as_str()), GmidxEnum::Enum(co::ENUM_SETTINGS::CURRENT), &mut dev_mode).is_ok() {
            let device_key: String = windows_display_state.DeviceKey();
            let position = Position(dev_mode.dmPosition());
            let resolution = Resolution::new(dev_mode.dmPelsWidth, dev_mode.dmPelsHeight);
            let orientation = Orientation::from_winsafe(dev_mode.dmDisplayOrientation()).expect("Failed to find corresponding orientation enum");
            let fixed_output = FixedOutput::from_winsafe(dev_mode.dmDisplayFixedOutput()).expect("Failed to find corresponding fixed output enum");
            let refresh_rate = RefreshRate(dev_mode.dmDisplayFrequency);

            return Some(DisplayState {
                device_key,
                position,
                resolution,
                refresh_rate,
                fixed_output,
                orientation,
                is_primary: windows_display_state.StateFlags.has(co::DISPLAY_DEVICE::PRIMARY_DEVICE),
                is_enabled: windows_display_state.StateFlags.has(co::DISPLAY_DEVICE::ATTACHED_TO_DESKTOP) &&
                    !windows_display_state.StateFlags.has(co::DISPLAY_DEVICE::DISCONNECT)
            });
        }

        return None;
    }
}

impl Hash for DisplayState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.device_key.hash(state);
    }
}
