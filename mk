#! /bin/sh


THINGS_TODO=$*
if [ $# -eq 0 ]; then
    THINGS_TODO="rebuild"
fi


for todo in $THINGS_TODO ; do
    case "$todo" in
        "rebuild" )
            echo "Rebuilding"
            ocaml setup.ml -build
            ocaml setup.ml -uninstall
            ocaml setup.ml -install ;;
        * ) echo "Can't understand \`$todo'";;
    esac
done
