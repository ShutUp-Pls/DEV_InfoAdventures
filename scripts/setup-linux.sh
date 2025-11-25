#!/usr/bin/env bash

if command -v apt >/dev/null 2>&1; then
    sudo apt install -y libsdl2-dev libsdl2-ttf-dev pkg-config

elif command -v pacman >/dev/null 2>&1; then
    sudo pacman -S --noconfirm sdl2 sdl2_ttf pkgconf

elif command -v dnf >/dev/null 2>&1; then
    sudo dnf install -y SDL2-devel SDL2_ttf-devel pkgconf

else
    echo "Distribución no soportada, instala SDL2 y SDL2_ttf manualmente."
fi