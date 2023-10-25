#[cfg(feature = "config-json")]
#[macro_export]
macro_rules! if_config_json {
    ($($stuff:tt)*) => {$($stuff)*}
}
#[cfg(not(feature = "config-json"))]
#[macro_export]
macro_rules! if_config_json {
    ($($stuff:tt)*) => {};
}
#[cfg(not(feature = "config-json"))]
#[macro_export]
macro_rules! if_not_config_json {
    ($($stuff:tt)*) => {$($stuff)*}
}
#[cfg(feature = "config-json")]
#[macro_export]
macro_rules! if_not_config_json {
    ($($stuff:tt)*) => {};
}
#[cfg(feature = "config-toml")]
#[macro_export]
macro_rules! if_config_toml {
    ($($stuff:tt)*) => {$($stuff)*}
}
#[cfg(not(feature = "config-toml"))]
#[macro_export]
macro_rules! if_config_toml {
    ($($stuff:tt)*) => {};
}
#[cfg(not(feature = "config-toml"))]
#[macro_export]
macro_rules! if_not_config_toml {
    ($($stuff:tt)*) => {$($stuff)*}
}
#[cfg(feature = "config-toml")]
#[macro_export]
macro_rules! if_not_config_toml {
    ($($stuff:tt)*) => {};
}
pub const CONFIG_JSON: bool = cfg!(feature = "config-json");
pub const CONFIG_TOML: bool = cfg!(feature = "config-toml");
