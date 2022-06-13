#pragma once

#define MASTER_RIGHT

#define USE_SERIAL_PD2

#define TAPPING_FORCE_HOLD
#define TAPPING_TERM 175
#define AUTO_SHIFT_TIMEOUT 150
#define AUTO_SHIFT_NO_AUTO_REPEAT

// Prevent normal rollover on alphas from accidentally triggering mods.
#define IGNORE_MOD_TAP_INTERRUPT

#define SPLIT_OLED_ENABLE
#define OLED_FONT_H "keyboards/crkbd/lib/glcdfont.c"

// otimizations
#define NO_ACTION_MACRO
#define NO_ACTION_FUNCTION
#ifndef NO_DEBUG
#define NO_DEBUG
#endif // !NO_DEBUG
#if !defined(NO_PRINT) && !defined(CONSOLE_ENABLE)
#define NO_PRINT
#endif // !NO_PRINT
#define NO_ACTION_ONESHOT
#define NO_MUSIC_MODE
