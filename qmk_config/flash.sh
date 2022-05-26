#!/run/current-system/sw/bin/env nix-shell
#!nix-shell -i bash -p avrdude


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
	    "corne")
		echo "TODO"
		;;
	    "lily")
		mkdir -p .build
		echo "Building firmware"
		${containerRunner} run \
				--rm \
				-it \
				-v $(pwd)/.build:/build_result \
				-v $(pwd)/lily58:/qmk_firmware/keyboards/lily58/keymaps/tmp_123456:ro \
				qmk-config \
				/bin/bash -c 'make lily58:tmp_123456 && mv lily58_rev1_tmp_123456.hex /build_result/lily58.hex' &> /dev/null
		if [[ $? != 0 ]]; then
		    echo "Error during the build of the firmware"
		    exit 1
		fi
		echo "Waiting the keyboard in reset mode"
		waitTime=0
		while true; do
		    avrdude -v -patmega32u4 -cavr109 -P/dev/ttyACM0 -b57600 -Uflash:w:".build/lily58.hex":i &> /dev/null
		    if [[ $? == 0 ]]; then
			echo "Done"
			rm -r .build
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
