#!/usr/bin/env bash

podman --help &> /dev/null
isPodman=$?
if [[ $isPodman == 0 ]]; then
    containerRunner="podman"
else
    containerRunner="docker"
fi

helpMessage="usage: ${0}\n
\tbuild: build the qmk docker/podman container"

case $1 in
    "build")
	${containerRunner} build -t qmk-config .
	;;
    "flash")
	case $2 in
	    "corne-left")
		mkdir -p .build
		mkdir -p /tmp/qmk_config/
		echo "Building firmware"
		${containerRunner} run \
				--rm \
				-it \
				-v $(pwd)/.build:/build_result \
				-v $(pwd)/corne-left:/qmk_firmware/keyboards/crkbd/keymaps/tmp_123456:ro \
				qmk-config \
				/bin/bash
				# /bin/bash -c 'qmk compile -kb crkbd -km tmp_123456 && mv corne-left_rev1_tmp_123456.hex /build_result/corne-left.hex' &> /tmp/qmk_config/log
		if [[ $? != 0 ]]; then
		    echo "Error during the build of the firmware"
		    cat /tmp/qmk_config/log
		    rm -r .build
		    rm -r /tmp/qmk_config
		    exit 1
		fi
		echo "Waiting the keyboard in reset mode"
		waitTime=0
		while true; do
		    doas dfu-programmer atmega32u4 erase
		    doas dfu-programmer atmega32u4 flash .build/corne-right.hex
		    doas dfu-programmer atmega32u4 reset
		    if [[ $? == 0 ]]; then
			echo "Done"
			rm -r .build
			rm -r /tmp/qmk_config
			exit 0
		    fi
		    case $waitTime in
			0)
			    echo -ne "\033[2K"
			    echo -ne '.\r'
			    waitTime=1
			    ;;
			1)
			    echo -ne "\033[2K"
			    echo -ne '..\r'
			    waitTime=2
			    ;;
			2)
			    echo -ne "\033[2K"
			    echo -ne '...\r'
			    waitTime=0
			    ;;
		    esac
		    sleep 0.5
		done
		;;
	    "corne-right")
		mkdir -p .build
		mkdir -p /tmp/qmk_config/
		echo "Building firmware"
		${containerRunner} run \
				--rm \
				-it \
				-v $(pwd)/.build:/build_result \
				-v $(pwd)/corne-right:/qmk_firmware/keyboards/crkbd/keymaps/tmp_123456:ro \
				qmk-config \
				/bin/bash -c 'qmk compile -kb crkbd -km tmp_123456 && mv crkbd_rev1_tmp_123456.hex /build_result/corne-right.hex' &> /tmp/qmk_config/log
		if [[ $? != 0 ]]; then
		    echo "Error during the build of the firmware"
		    cat /tmp/qmk_config/log
		    rm -r .build
		    rm -r /tmp/qmk_config
		    exit 1
		fi
		echo "Waiting the keyboard in reset mode"
		waitTime=0
		while true; do
		    doas dfu-programmer atmega32u4 erase
		    doas dfu-programmer atmega32u4 flash .build/corne-right.hex
		    doas dfu-programmer atmega32u4 reset
		    if [[ $? == 0 ]]; then
			echo "Done"
			rm -r .build
			rm -r /tmp/qmk_config
			exit 0
		    fi
		    case $waitTime in
			0)
			    echo -ne "\033[2K"
			    echo -ne '.\r'
			    waitTime=1
			    ;;
			1)
			    echo -ne "\033[2K"
			    echo -ne '..\r'
			    waitTime=2
			    ;;
			2)
			    echo -ne "\033[2K"
			    echo -ne '...\r'
			    waitTime=0
			    ;;
		    esac
		    sleep 0.5
		done
		;;
	    "lily")
		mkdir -p .build
		mkdir -p /tmp/qmk_config/
		echo "Building firmware"
		${containerRunner} run \
				--rm \
				-it \
				-v $(pwd)/.build:/build_result \
				-v $(pwd)/lily58:/qmk_firmware/keyboards/lily58/keymaps/tmp_123456:ro \
				qmk-config \
				/bin/bash -c 'qmk compile -kb lily58 -km tmp_123456 && mv lily58_rev1_tmp_123456.hex /build_result/lily58.hex' &> /tmp/qmk_config/log
		if [[ $? != 0 ]]; then
		    echo "Error during the build of the firmware"
		    cat /tmp/qmk_config/log
		    rm -r .build
		    rm -r /tmp/qmk_config
		    exit 1
		fi
		echo "Waiting the keyboard in reset mode"
		waitTime=0
		while true; do
		    avrdude -v -patmega32u4 -cavr109 -P/dev/ttyACM0 -b57600 -Uflash:w:".build/lily58.hex":i &> /dev/null
		    if [[ $? == 0 ]]; then
			echo "Done"
			rm -r .build
			rm -r /tmp/qmk_config
			exit 0
		    fi
		    case $waitTime in
			0)
			    echo -ne "\033[2K"
			    echo -ne '.\r'
			    waitTime=1
			    ;;
			1)
			    echo -ne "\033[2K"
			    echo -ne '..\r'
			    waitTime=2
			    ;;
			2)
			    echo -ne "\033[2K"
			    echo -ne '...\r'
			    waitTime=0
			    ;;
		    esac
		    sleep 0.5
		done
		;;
	    *)
		echo "supported keyboard corne or lily"
		;;
	esac
	;;
    *)
	echo -e ${helpMessage}
	;;
esac
