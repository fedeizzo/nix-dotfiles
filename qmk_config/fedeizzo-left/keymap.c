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
       LGUI(KC_TAB),    KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,                      KC_J,    KC_L,    KC_U,    KC_Y, KC_QUOT, LCTL(KC_T),
       LCTL_T(KC_ESC),    KC_A,    KC_R,    KC_S,KC_T,    KC_G,                      KC_M,    KC_N,    KC_E,    KC_I,    KC_O, KC_SCLN,
      KC_LSFT,    KC_Z,    KC_X,    KC_C,    KC_D,    KC_V,                      KC_K,    KC_H, KC_COMM,  KC_DOT, KC_SLSH,  LCTL(KC_TAB),
                              LT(3, KC_ESC), LT(1, KC_SPC), LGUI_T(KC_TAB),  LCTL_T(KC_ENT), LT(2, KC_BSPC), KC_RALT

  ),
  [_NAVIGATION] = LAYOUT_split_3x6_3(
      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
      XXXXXXX, XXXXXXX, XXXXXXX,BALL_PRC,BALL_SCR, BALL_LC,                      KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT, XXXXXXX, XXXXXXX,
      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                      KC_HOME,  KC_END, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
                                          XXXXXXX, _______, XXXXXXX,     KC_ENT, KC_BSPC, XXXXXXX
  ),
  [_NUM] = LAYOUT_split_3x6_3(
        RESET, KC_LBRC,    KC_7,    KC_8,    KC_9, KC_RBRC,                      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
      XXXXXXX, XXXXXXX,    KC_4,    KC_5,    KC_6,  KC_EQL,                      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
      XXXXXXX,  KC_GRV,    KC_1,    KC_2,    KC_3, KC_BSLS,                      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
                                          XXXXXXX,    KC_0, KC_MINS,    XXXXXXX, _______, XXXXXXX
  ),
  [_MEDIA] = LAYOUT_split_3x6_3(
      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                      KC_MPRV, KC_VOLD, KC_VOLU, KC_MNXT, XXXXXXX, XXXXXXX,
      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
                                          XXXXXXX, XXXXXXX, XXXXXXX,    XXXXXXX, KC_MPLY, KC_MUTE
  ),
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
