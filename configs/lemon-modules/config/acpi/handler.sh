#!/bin/bash
# Default acpi script that takes an entry for all actions

case "$1" in
    button/power)
        case "$2" in
            PBTN|PWRF)
                logger 'PowerButton pressed'
                ;;
            *)
                logger "Button/power ACPI action undefined: $2"
                ;;
        esac
        ;;
    button/sleep)
        case "$2" in
            SLPB|SBTN)
                logger 'SleepButton pressed'
                ;;
            *)
                logger "Button/sleep ACPI action undefined: $2"
                ;;
        esac
        ;;
    jack/headphone)
        case "$2" in
            HEADPHONE)
                logger 'Jack event.'
                runuser --login tibor -c 'lemon-modules-update --event volume --debug' 2>&1 | logger
                ;;
            *)
                logger "Jack ACPI action undefined: $2"
                ;;
        esac
        ;;
    ac_adapter)
        case "$2" in
            AC|ACAD|ADP0)
                case "$4" in
                    00000000)
                        logger 'AC unpluged'
                        ;;
                    00000001)
                        logger 'AC pluged'
                        ;;
                esac
                ;;
            *)
                logger "AC adapter ACPI action undefined: $2"
                ;;
        esac
        ;;
    battery)
        case "$2" in
            BAT0)
                case "$4" in
                    00000000)
                        logger 'Battery online'
                        ;;
                    00000001)
                        logger 'Battery offline'
                        ;;
                esac
                ;;
            CPU0)
                ;;
            *)  logger "Battery ACPI action undefined: $2" ;;
        esac
        ;;
    button/lid)
        case "$3" in
            close)
                logger 'LID closed imre'
                ;;
            open)
                logger 'LID opened imre'
                runuser --login tibor -c 'lemon-modules-update --event lid_open --debug' 2>&1 | logger
                ;;
            *)
                logger "Button lid ACPI action undefined: $3"
                ;;
    esac
    ;;
    *)
        logger "ACPI group/action undefined: $1 / $2"
        ;;
esac

# vim:set ts=4 sw=4 ft=sh et:
