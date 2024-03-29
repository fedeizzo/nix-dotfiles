AUDIO_ENABLE = no
AUTO_SHIFT_ENABLE = no
BLUETOOTH_ENABLE = no
CONSOLE_ENABLE = no
EXTRAFLAGS+=-flto
OLED_DRIVER = SSD1306
OLED_ENABLE = yes
SPACE_CADET_ENABLE=no
SPLIT_KEYBOARD = yes
TAP_DANCE_ENABLE = no
WPM_ENABLE = yes
# COMBO_ENABLE = yes
# VPATH += keyboards/gboards

PIMORONI_TRACKBALL_ENABLE = yes
ifeq ($(strip $(PIMORONI_TRACKBALL_ENABLE)), yes)
        POINTING_DEVICE_ENABLE = yes
        POINTING_DEVICE_DRIVER = pimoroni_trackball
        PIMORONI_TRACKBALL_ADDRESS = 0x0B
        SRC += drivers/sensors/pimoroni_trackball.c
        QUANTUM_LIB_SRC += i2c_master.c
endif
