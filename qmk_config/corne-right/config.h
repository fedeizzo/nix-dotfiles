#pragma once

#define MASTER_RIGHT

// Pimoroni trackball
#define PIMORONI_TRACKBALL_ENABLE
#define PIMORONI_TRACKBALL_CLICK
#define PIMORONI_TRACKBALL_ROTATE
// #define PIMORONI_TRACKBALL_INVERT_X

#define USE_SERIAL_PD2

// Combo feature timeout (defaults to TAPPING_TERM)
// #define COMBO_TERM 50
// Size of combo_events enum.
// #define COMBO_VARIABLE_LEN

#define TAPPING_FORCE_HOLD
#define TAPPING_TERM 175
#define AUTO_SHIFT_TIMEOUT 175
#define AUTO_SHIFT_REPEAT
#define NO_AUTO_SHIFT_SPECIAL
#define NO_AUTO_SHIFT_NUMERIC

// Prevent normal rollover on alphas from accidentally triggering mods.
#define IGNORE_MOD_TAP_INTERRUPT

#define SPLIT_OLED_ENABLE
#define OLED_FONT_H "keyboards/crkbd/lib/glcdfont.c"

// otimizations
#define NO_ACTION_MACRO
#define NO_ACTION_FUNCTION
#ifndef NO_DEBUG
#    define NO_DEBUG
#endif  // !NO_DEBUG
#if !defined(NO_PRINT) && !defined(CONSOLE_ENABLE)
#    define NO_PRINT
#endif  // !NO_PRINT
#define NO_ACTION_ONESHOT
#define NO_MUSIC_MODE
