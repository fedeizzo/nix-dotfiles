#include QMK_KEYBOARD_H
#include <stdio.h>
#include "keymap_us_international.h"

#ifdef OLED_ENABLE
#include "oled.c"
#endif

enum crkbd_layers {
    _BASE,
    _NAVIGATION,
    _NUM,
    _MEDIA
};

enum trackball_keycodes {
    BALL_LC = SAFE_RANGE,
    BALL_SCR,
    BALL_PRC,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [_BASE] = LAYOUT_split_3x6_3(
    LGUI(KC_TAB),KC_Q,KC_W,KC_F,KC_P,KC_B,KC_J,KC_L,KC_U,KC_Y,KC_QUOT,LCTL(KC_T),
    KC_LCTL,KC_A,KC_R,KC_S,KC_T,KC_G,KC_M,KC_N,KC_E,KC_I,KC_O,KC_SCLN,
    KC_CAPS,KC_Z,KC_X,KC_C,KC_D,KC_V,KC_K,KC_H,KC_COMMA,KC_DOT,KC_SLSH,KC_L+TAB,
    LT(_NAV, KC_ESC),LGUI_T(KC_SPC),SFT_T(KC_TAB),LCTL_T(KC_ENT),LT(_NUM, KC_BSPC),KC_LALT
  ),
  [_NAVIGATION] = LAYOUT_split_3x6_3(
    KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,KC_MPRV,KC_VOLD,KC_VOLU,KC_MNXT,KC_NO,KC_NO,
    KC_NO,KC_NO,KC_NO,BALL_PRC,BALL_SRC,BALL_LC,KC_LEFT,KC_DOWN,KC_UP,KC_RGHT,KC_NO,KC_NO,
    KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,KC_HOME,KC_END,KC_MPLY,KC_MUTE,KC_NO,KC_NO,
    LT(_NAV, KC_ESC),LGUI_T(KC_SPC),SFT_T(KC_TAB),LCTL_T(KC_ENT),LT(_NUM, KC_BSPC),KC_LALT
  ),
  [_NUM] = LAYOUT_split_3x6_3(
    KC_RESET,KC_LBRC,KC_7,KC_8,KC_9,KC_RBRC,KC_LCBR,KC_LPRN,KC_ASTR,KC_AMPR,KC_RCBR,KC_NO,
    KC_NO,KC_0,KC_4,KC_5,KC_6,KC_EQL,KC_PLUS,KC_CIRC,KC_PERC,KC_DLR,KC_RPRN,KC_NO,
    KC_NO,KC_GRV,KC_1,KC_2,KC_3,KC_BSLS,KC_PIPE,KC_HASH,KC_AT,KC_EXLM,KC_TILD,KC_NO,
    KC_UNDS,KC_SPC,KC_MINS,KC_ENT,KC_TRNS,KC_NO
  )
};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    switch (keycode){
        default:
#ifdef OLED_ENABLE
        if (record->event.pressed) {
            oled_timer = timer_read();
        }
#endif
        break;
    }
  return true;
}

void keyboard_post_init_user(void) {
}
