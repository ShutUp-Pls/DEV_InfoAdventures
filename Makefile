# Nombre del ejecutable
EXE_NAME = DEV-InfoAdventures
CABAL = cabal

# Colores para la consola
GREEN  := $(shell tput -Txterm setaf 2)
RED    := $(shell tput -Txterm setaf 1)
YELLOW := $(shell tput -Txterm setaf 3)
RESET  := $(shell tput -Txterm sgr0)

# Detección de Sistema Operativo
ifeq ($(OS),Windows_NT)
    DETECTED_OS := Windows
else
    DETECTED_OS := $(shell uname -s)
endif

.PHONY: all build run clean repl setup check check-os check-tools check-libs help

all: build

# 1. Chequeo principal
check: check-os check-tools check-libs
	@echo "${GREEN}✔ Todo parece correcto. Ya puedes ejecutar 'make run'.${RESET}"

# 2. Informar OS detectado
check-os:
	@echo "${YELLOW}--- Inspeccionando Sistema (${DETECTED_OS}) ---${RESET}"

# 3. Verificar GHC y Cabal
check-tools:
	@which ghc > /dev/null || (echo "${RED}(X) Error: GHC no instalado.${RESET} Instala GHCup: https://www.haskell.org/ghcup/"; exit 1)
	@echo "${GREEN}(✔) GHC encontrado.${RESET}"
	@which cabal > /dev/null || (echo "${RED}(X) Error: Cabal no instalado.${RESET} Instala GHCup."; exit 1)
	@echo "${GREEN}(✔) Cabal encontrado.${RESET}"

# 4. Verificar Librerías de Sistema (SDL2)
check-libs:
ifeq ($(DETECTED_OS),Windows)
	@# Verificación para Windows (MSYS2 / MinGW64)
	@pacman -Q mingw-w64-x86_64-sdl2 > /dev/null 2>&1 || \
		(echo "${RED}(X) Error: SDL2 no detectado.${RESET}\n   Ejecuta en tu terminal MSYS2:\n   ${YELLOW}pacman -S mingw-w64-x86_64-sdl2 mingw-w64-x86_64-sdl2-image mingw-w64-x86_64-sdl2-ttf${RESET}"; exit 1)
	@echo "${GREEN}(✔) Librerías SDL2 (MinGW64) detectadas.${RESET}"
else
	@# Verificación para Linux (Debian/Ubuntu/Fedora)
	@pkg-config --exists sdl2 || \
		(echo "${RED}(X) Error: SDL2 no detectado.${RESET}\n   En Ubuntu/Debian ejecuta:\n   ${YELLOW}sudo apt-get install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev${RESET}"; exit 1)
	@pkg-config --exists SDL2_image || (echo "${RED}(X) Falta SDL2_image.${RESET}"; exit 1)
	@pkg-config --exists SDL2_ttf || (echo "${RED}(X) Falta SDL2_ttf.${RESET}"; exit 1)
	@echo "${GREEN}(✔) Librerías SDL2 (Linux) detectadas.${RESET}"
endif

setup: check
	@echo "${YELLOW}--- Instalando dependencias de Haskell ---${RESET}"
	$(CABAL) update
	$(CABAL) build --only-dependencies
	@echo "${GREEN}(✔) Setup completado.${RESET}"

build:
	$(CABAL) build

run:
	$(CABAL) run

repl:
	$(CABAL) repl

clean:
	$(CABAL) clean

help:
	@echo "Comandos disponibles:"
	@echo "  ${YELLOW}make check${RESET}  - Diagnostica si tienes todo instalado."
	@echo "  ${YELLOW}make setup${RESET}  - Verifica e instala dependencias del proyecto."
	@echo "  ${YELLOW}make run${RESET}    - Compila y juega."
