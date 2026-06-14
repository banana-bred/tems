# tems — Terminal ElectroMagnetic Spectrum reference

Print the electromagnetic specturm in your terminal.
Displays wavelengths, frequencies, and energies, as well as the rough color breakdown of the optical (visible) band.

## Installing
This is a very simply program, consisting of a single Fortran program file.
Just use your favorite fortran compiler\*

`gcc -ffree-form main/tems.f -o tems`

You can also use

`make`

This is also installable with [*<span style="text-decoration: underline">Fortran Package Manager (fpm)</span>*](https://github.com/fortran-lang/fpm)

`fpm install`

\* The code uses `isatty()` to check whether the output unit is attached to a terminal.
Most major compilers ship this as an extension, I think.

## Usage

- Print the electromagnetic spectrum:

`tems`

- Print the electromagnetic spectrum in reverse:

`tems --reverse`

- Print the electromagnetic spectrum without printing the breakdown of visible light into colors:

`tems --novis`

- Ask for the band containing a specific frequency, wavelength, or energy of light

```
tems 21 cm
tems 5.9e-6 eV
tems 1.43 GHz
```

- Ask for help

`tems -h`
