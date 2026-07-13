! ================================================================================================================================ !
program tems
  !! tems - Terminal ElectroMagnetic Spctrum reference
  !!
  !!   A quick reference table for the electromagnetic spectrum.
  !!   Shows wavelength, frequency and photon energy ranges for each named band.

  use, intrinsic :: iso_fortran_env, only: stdout => output_unit, stderr => error_unit

  implicit none(type, external)

  integer, parameter :: dp = selected_real_kind(15)

  character(*), parameter :: PROGNAME = 'tems'

  ! -- physical constants (SI)
  real(dp), parameter :: c_ms = 2.99792458e8_dp      !! speed of light (m/s)
  real(dp), parameter :: h_Js = 6.62607015e-34_dp    !! Planck, J*s
  real(dp), parameter :: J2eV = 1.602176634e-19_dp   !! Joule -> eV

  ! -- ansi_colors
  character(*), parameter :: GREY    = '38;5;244'
  character(*), parameter :: TEAL    = '38;5;37'
  character(*), parameter :: RED     = '38;5;124'
  character(*), parameter :: WHITE   = '38;5;231'
  character(*), parameter :: VIOLET  = '38;5;141'
  character(*), parameter :: BLUE    = '38;5;33'
  character(*), parameter :: MAGENTA = '38;5;201'

  ! -- names
  character(*), parameter :: RD = "Radio"
  character(*), parameter :: MW = "Microwave"
  character(*), parameter :: IR = "Infrared"
  character(*), parameter :: VI = "Visible"
  character(*), parameter :: UV = "Ultraviolet"
  character(*), parameter :: XR = "X-ray"
  character(*), parameter :: GR = "Gamma-ray"

  ! -- bands
  integer, parameter :: NBANDS = 7
  character(len=12), parameter :: BNAME(*) = [character(len=12) :: RD,   MW,   IR,  VI,    UV,     XR,   GR]
  character(len=8),  parameter :: BCOL(*)  = [character(len=8)  :: GREY, TEAL, RED, WHITE, VIOLET, BLUE, MAGENTA]
  real(dp), parameter :: LAM_LO(*) = [1.0e-1_dp, 1.0e-3_dp, 7.0e-7_dp, 4.0e-7_dp, 1.0e-8_dp, 1.0e-11_dp, 1.0e-14_dp] !! λ lbound
  real(dp), parameter :: LAM_HI(*) = [1.0e5_dp,  1.0e-1_dp, 1.0e-3_dp, 7.0e-7_dp, 4.0e-7_dp, 1.0e-8_dp,  1.0e-11_dp] !! λ ubound

  ! -- table
  integer,      parameter :: TBL_WIDTH = 66
  character(*), parameter :: TBL_SEP   = ' | '
  character(*), parameter :: TBL_END   = ' |'

  ! -- classification
  integer, parameter :: CLASS_WIDTH = 40

  integer, parameter :: CFORCE_NEVER  = -1
  integer, parameter :: CFORCE_AUTO   = 0
  integer, parameter :: CFORCE_ALWAYS = 1
  integer :: cforce                   !! color force: -1 never, 0 auto, +1 always

  logical :: show_optical_breakdown
  character(len=64) :: arg
  character(len=64) :: env
  character(len=64) :: pos(2)
  integer :: nargs, iarg
  integer :: envlen, envstat
  integer :: npos
  logical :: flip                     !! .true. = radio at bottom
  logical :: use_color                !! master color switch

  ! ============================================================================================================================== !
  ! ============================================================================================================================== !
  ! ============================================================================================================================== !

  flip   = .false.   ! default: radio at TOP (long wavelength first)
  pos    = ''
  npos   = 0
  cforce = CFORCE_AUTO
  show_optical_breakdown = .true.

  ! -- argument parsing
  nargs = command_argument_count()
  do iarg = 1, nargs
    call get_command_argument(iarg, arg)
    select case (trim(arg))
    case ('-h', '--help', 'help')
      call print_help()
      stop
    case ('-r', '--reverse', '-f', '--flip')
      flip = .true.
    case('--novis', '--no-vis')
      show_optical_breakdown = .false.
    case ('--color=never','--no-color','--nocolor')
      cforce = CFORCE_NEVER
    case ('--color=always','--color')
      cforce = CFORCE_ALWAYS
    case ('--color=auto')
      cforce = CFORCE_AUTO
    case default
      if (npos .ge. 2) cycle
      npos = npos + 1
      pos(npos) = arg
    end select
  end do

  ! -- decide whether to emit color, with priorities:
  !        explicit flag  >  NO_COLOR env  >  isatty(stdout)
  select case(cforce)
  case(CFORCE_ALWAYS)
    use_color = .true.
  case(CFORCE_NEVER)
    use_color = .false.
  case default
    ! auto can also honor the  NO_COLOR environmental variable, then falling back to terminal detection
    call get_environment_variable('NO_COLOR', env, envlen, envstat)
    if (envstat .eq. 0 .AND. envlen .gt. 0) then
      ! -- no-color is set and nonempty
      use_color = .false.
    else
      ! -- only set color for terminals
      use_color = isatty(stdout)
    end if
  end select

  ! -- dispatch
  if(npos .eq. 0) then
    call print_table()
  elseif(npos .eq. 2) then
    call classify(pos(1), pos(2))
  ! -- vvvv errors vvvv -- !
  elseif(npos .eq. 1) then
    write(stderr, '(A)') 'ERROR: only one positional parameter detected: ' // trim(pos(1))
    write(stderr, '(A)') 'Please provide a value AND a unit, or nothing at all. '
    write(stderr, '(A)') PROGNAME//' -h for help.'
    stop 1, quiet=.true.
  elseif(npos .gt. 2) then
    write(stderr, '(A)') 'ERROR: more than 2 positional parameters detected'
    write(stderr, '(A)') 'Please provide either nothing or exactly one value and then exactly one unit'
    write(stderr, '(A)') PROGNAME//' -h for help.'
    stop 1, quiet=.true.
  else
    write(stderr, '(A)') "ERROR: negative NPOS detected. I don't know what to do with that"
    write(stderr, '(A)') PROGNAME//' -h for help.'
    stop 1, quiet=.true.
  end if

! ================================================================================================================================ !
contains
! ================================================================================================================================ !

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine print_table
    integer :: j
    write(stdout,*)
    write(stdout,'(A)') bold('  Electromagnetic Spectrum')
    write(stdout,*)
    write(stdout,'(A)') '  '//repeat('-', TBL_WIDTH)
    write(stdout,'(A)') '  '// &
      bold('        Band')//TBL_SEP// &
      bold('     Wavelength        ')//TBL_SEP// &
      bold('     Frequency         ')//TBL_END
    write(stdout,'(A)') '  '// &
      '            '//TBL_SEP// &
      '                       '//TBL_SEP// &
      bold('   Photon energy       ')//TBL_END
    write(stdout,'(A)') '  '//repeat('-', TBL_WIDTH)
    do j = 1, NBANDS
      if(flip) then
        call print_band(NBANDS - j + 1)
      else
        call print_band(j)
      end if
    end do
    write(stdout,*)
    if(show_optical_breakdown .eqv. .false.) return
    call print_visible
    write(stdout,*)
  end subroutine print_table

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine print_visible
    !! colored breakdown of the visible band
    integer, parameter :: NV = 7
    character(*), parameter :: CR= '38;5;196'
    character(*), parameter :: CO= '38;5;208'
    character(*), parameter :: CY= '38;5;226'
    character(*), parameter :: CG= '38;5;46'
    character(*), parameter :: CB= '38;5;27'
    character(*), parameter :: CC= '38;5;51'
    character(*), parameter :: CV= '38;5;57'
    character(*), parameter :: WR = '625-700 nm'
    character(*), parameter :: WO = '590-625 nm'
    character(*), parameter :: WY = '565-590 nm'
    character(*), parameter :: WG = '500-565 nm'
    character(*), parameter :: WB = '450-485 nm'
    character(*), parameter :: WC = '485-500 nm'
    character(*), parameter :: WV = '380-450 nm'
    character(len=8)  :: vc(NV)
    character(len=8)  :: vn(NV)
    character(len=12) :: vr(NV)
    integer :: j
    vn = [character(len=8) :: 'Violet','Blue','Cyan','Green','Yellow','Orange','Red']
    vr = [character(len=12) :: WV, WB, WC, WG, WY, WO, WR]
    vc = [character(len=8)  :: CV, CB, CC, CG, CY, CO, CR]
    write(stdout,'(A)') '  '//bold('Visible light breakdown')//' (approximately):'
    do j = 1, NV
      ! -- color block + name + λ range
      write(stdout,'(A)') '    '// &
        paint('48;5;'//trim(adjustl(vc(j)(6:))), '   ')//' '// &
        paint(trim(vc(j)), vn(j))//' '//trim(vr(j))
    end do
  end subroutine print_visible

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine print_band(k)
    !! Print a band of the EM spectrum
    integer, intent(in) :: k
    real(dp) :: f_lo, f_hi, e_lo, e_hi
    ! -- frequency from wavelength: f = c / λ
    f_lo = c_ms / LAM_HI(k)
    f_hi = c_ms / LAM_LO(k)
    ! -- photon energy: E = h f, (eV)
    e_lo = h_Js * f_lo / J2eV
    e_hi = h_Js * f_hi / J2eV

    ! -- color code and reset as separate items so that the visible terminal width is constant
    write(stdout, '(A, A, A12, A, A, A23, A, A23, A)') '  ',        &
      copen(trim(BCOL(k))//';1'), BNAME(k), creset(),      TBL_SEP, & ! Band
      pretty(LAM_HI(k),'m')//' - '//pretty(LAM_LO(k),'m'), TBL_SEP, & ! λ
      pretty(f_lo,'Hz')//' - '//pretty(f_hi,'Hz'),         TBL_END    ! frequency

    write(stdout, '(A, A12, A, A23, A, A23, A)') '  ', &
      '', TBL_SEP,                                     &   ! blank
      '', TBL_SEP,                                     &   ! blank
      pretty(e_lo,'eV')//' - '//pretty(e_hi,'eV'), TBL_END ! energy

    write(stdout,'(A)') '  '//repeat('-', TBL_WIDTH) ! hline

  end subroutine print_band

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function paint(code, s) result(r)
    !! Wrap a string in an ANSI SGR code, or return it unchanged when
    !! color output is disabled. The char 'code' is the part between ESC[ and m, e.g., '1' for bold
    !! or '38;5;201' for a 256-color
    character(len=*), intent(in)  :: code, s
    character(len=:), allocatable :: r
    r = s
    if(use_color) r = char(27)//'['//trim(code)//'m'//r//char(27)//'[0m'
  end function paint

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function bold(s) result(r)
    !! Returns bold text
    character(len=*), intent(in)  :: s
    character(len=:), allocatable :: r
    r = paint('1', s)
  end function bold

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function copen(code) result(r)
    !! Return open escape for a color code if we're using color, nothing otherwise
    character(len=*), intent(in)  :: code
    character(len=:), allocatable :: r
    r = ''
    if(use_color) r = char(27)//'['//trim(code)//'m'
  end function copen

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function creset() result(r)
    !! Return color reset if we're using color, nothing otherwise
    character(len=:), allocatable :: r
    r = ''
    if(use_color) r = char(27)//'[0m'
  end function creset

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function pretty(val, unit) result(s)
    !! Format a value with an SI prefix and unit
    real(dp),         intent(in) :: val
    character(len=*), intent(in) :: unit
    character(len=10) :: s
    real(dp) :: v
    integer :: e3
    character(len=2) :: pfx
    character(len=20) :: tmp

    if(val .eq. 0.0_dp) then
      s = '0'
      return
    end if

    ! -- find exponent (multiple of 3)
    e3 = floor(log10(val)/3.0_dp)*3
    v  = val / 10.0_dp**e3

    select case (e3)
    case (-15); pfx = 'f'
    case (-12); pfx = 'p'
    case ( -9); pfx = 'n'
    case ( -6); pfx = 'u'
    case ( -3); pfx = 'm'
    case (  0); pfx = ' '
    case (  3); pfx = 'k'
    case (  6); pfx = 'M'
    case (  9); pfx = 'G'
    case ( 12); pfx = 'T'
    case ( 15); pfx = 'P'
    case ( 18); pfx = 'E'
    case ( 21); pfx = 'Z'
    case ( 24); pfx = 'Y'
    case default
      ! -- fall back to scientific notation if we're out of the predetermined prefix range
      write(tmp,'(ES10.4)') val
      s = adjustl(trim(tmp)) // unit
      return
    end select

    write(tmp,'(F7.3)') v
    s = trim(tmp) // ' ' // trim(pfx) // unit

  end function pretty

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine classify(vstr, ustr)
    !! Determine which band this falls into. Accepts wavelength, energy, or frequency in 'vstr'
    character(len=*), intent(in) :: vstr !! value string, e.g., 500
    character(len=*), intent(in) :: ustr !! units string, e.g., nm
    real(dp) :: val, lam, freq, en
    integer :: ios, k
    character(len=:), allocatable :: u

    read(vstr, *, iostat=ios) val
    if (ios .ne. 0) then
      write(stderr,'(A)') 'ERROR: could not parse value: ' // trim(vstr)
      stop 1, quiet = .true.
    end if
    u = trim(ustr)

    ! -- convert whatever was given into a wavelength in meters.
    select case (lower(u))
    ! -- λ
    case ('m')        ; lam = val
    case ('cm')       ; lam = val*1.0e-2_dp
    case ('mm')       ; lam = val*1.0e-3_dp
    case ('um','µm')  ; lam = val*1.0e-6_dp
    case ('nm')       ; lam = val*1.0e-9_dp
    case ('pm')       ; lam = val*1.0e-12_dp
    case ('A',&
          'ang',&
          'angstrom') ; lam = val*1.0e-10_dp
    ! -- frequency
    case ('hz')  ; lam = c_ms/val
    case ('khz') ; lam = c_ms/(val*1.0e3_dp)
    case ('mhz') ; lam = c_ms/(val*1.0e6_dp)
    case ('ghz') ; lam = c_ms/(val*1.0e9_dp)
    case ('thz') ; lam = c_ms/(val*1.0e12_dp)
    ! -- energy
    case ('ev')  ; lam = h_Js*c_ms/(val*J2eV)
    case ('kev') ; lam = h_Js*c_ms/(val*1.0e3_dp*J2eV)
    case ('mev') ; lam = h_Js*c_ms/(val*1.0e6_dp*J2eV)
    case ('j')   ; lam = h_Js*c_ms/val
    case default
      write(stderr, '(A)') 'ERROR: unknown unit: ' // u
      write(stderr, '(A)') 'Try -h for the unit list.'
      stop 1, quiet = .true.
    end select

    freq = c_ms / lam
    en   = h_Js * freq / J2eV

    write(stdout, *)
    write(stdout, '(A)')   '  Input: '//trim(vstr)//' '//u
    write(stdout, '(A)')   '  '//repeat('-', CLASS_WIDTH)
    write(stdout, '(A,A)') '   Wavelength : ', pretty(lam,'m')
    write(stdout, '(A,A)') '   Frequency  : ', pretty(freq,'Hz')
    write(stdout, '(A,A)') '   Energy     : ', pretty(en,'eV')
    ! -- which band ?
    do k = 1, NBANDS
      if(lam .lt. LAM_LO(k)) cycle
      if(lam .ge. LAM_HI(k)) cycle
      write(stdout,'(A,A)') '   Band       : ', paint(trim(BCOL(k))//';1', trim(BNAME(k)))
      exit
    end do
    if (lam .ge. LAM_HI(1))      write(stdout,'(A)') '   Band       : Radio (very long wavelength)'
    if (lam .lt. LAM_LO(NBANDS)) write(stdout,'(A)') '   Band       : Gamma-ray (very short wavelength)'
    write(stdout,*)
  end subroutine classify

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure function lower(chr) result(res)
    !! returns a lower case character
    implicit none (type, external)
    character(*), intent(in) :: chr
    character(:), allocatable :: res
    integer, parameter :: shift = ichar('a') - ichar("A")
    integer, parameter :: uppercase_a = ichar('A')
    integer, parameter :: uppercase_z = ichar('Z')
    integer :: i, n, ic
    n = len(chr)
    res = chr
    do i = 1, n
      ic = ichar(res(i:i))
      ! -- cycle if the character isn't in [A,Z]
      if(ic .lt. uppercase_a) cycle
      if(ic .gt. uppercase_z) cycle
      res(i:i) = char(ic + shift)
    enddo
  end function lower

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine print_help
    write(stdout,*)
    write(stdout,'(2X,A,A)') PROGNAME, ' :: Terminal ElectroMagnetic Spectrum reference'
    write(stdout,*)
    write(stdout,'(A)') 'USAGE'
    write(stdout,*)
    write(stdout,'(2X, A, A)') PROGNAME, '              print the full band table'
    write(stdout,'(2X, A, A)') PROGNAME, ' -r,--reverse same, but radio at the bottom'
    write(stdout,'(2X, A, A)') PROGNAME, ' --novis      do not print the optical breakdown'
    write(stdout,'(2X, A, A)') PROGNAME, ' <val> <unit> classify a single point'
    write(stdout,'(2X, A, A)') PROGNAME, ' -h           this help'
    write(stdout,*)
    write(stdout,'(A)') 'COLOR'
    write(stdout,*)
    write(stdout,'(A)') '  color is shown automatically when writing to a'
    write(stdout,'(A)') '  terminal, and suppressed when piped or redirected.'
    write(stdout,'(A)') '  --color=never (--no-color)  force color off'
    write(stdout,'(A)') '  --color=always              force color on (e.g. | less -R)'
    write(stdout,'(A)') '  --color=auto                default behavior'
    write(stdout,'(A)') '  the NO_COLOR env var also disables color.'
    write(stdout,*)
    write(stdout,'(A)') 'UNITS'
    write(stdout,*)
    write(stdout,'(A)') '  wavelength: m cm mm um nm pm A'
    write(stdout,'(A)') '  frequency : Hz kHz MHz GHz THz'
    write(stdout,'(A)') '  energy    : eV keV MeV J'
    write(stdout,*)
    write(stdout,'(A)') 'EXAMPLES'
    write(stdout,'(2X, A)') PROGNAME
    write(stdout,'(2X, A, A)') PROGNAME, ' 532 nm'
    write(stdout,'(2X, A, A)') PROGNAME, ' 2.45 GHz'
    write(stdout,'(2X, A, A)') PROGNAME, ' 10 keV'
    write(stdout,*)
  end subroutine print_help

! ================================================================================================================================ !
end program tems
! ================================================================================================================================ !
