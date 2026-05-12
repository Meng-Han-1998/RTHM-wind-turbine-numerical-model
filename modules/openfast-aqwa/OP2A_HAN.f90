SUBROUTINE OP2A_HAN(MODE,STAGE,TIME,TIMESTEP,COG,POSITION,VELOCITY,FORCE)

!DECLARATION TO MAKE USER_FORCE PUBLIC WITH UN-MANGLED NAME 

!DEC$ ATTRIBUTES C,DLLEXPORT :: OP2A_HAN
!DEC$ ATTRIBUTES REFERENCE :: MODE, STAGE
!DEC$ ATTRIBUTES REFERENCE :: TIME, TIMESTEP
!DEC$ ATTRIBUTES REFERENCE :: COG
!DEC$ ATTRIBUTES REFERENCE :: POSITION, VELOCITY, FORCE   

IMPLICIT NONE

INTEGER MODE,STAGE
REAL TIME, TIMESTEP
REAL, DIMENSION (3) :: COG
REAL, DIMENSION (6) :: POSITION, VELOCITY, FORCE
!
INTEGER             :: UnPIn = 10001          ! I/O unit of the primary input file (InputFileForFAST2AQWA.txt). 
INTEGER,SAVE        :: IndexTwrStr            ! - Index of the structure connecting to the rotor directly. 
REAL, DIMENSION (3) ,SAVE :: AddedPtfmFrc           ! Platform loads due to the turbine 
REAL, DIMENSION (3) ,SAVE :: AddedPtfmMom

CHARACTER(1000)     :: InputFile_AQWA                         ! String to hold the intput file name
INTEGER             :: UnOuAddedPtfm     = 10007              ! I/O unit of the output file containing the Platform loads due to the turbine in the global coordinate system
INTEGER             :: UnOuFORCE  = 10008                     ! I/O unit of the output file containing the external forces acting at the platform in the global coordinate system
INTEGER             :: I                                      ! Indicator for calculation

!------------------------------------------------------------------------
! MODE=0 - Initialise any summing variables/open/create files.
!          This mode is executed once before the simulation begins.
!------------------------------------------------------------------------

IF (MODE.EQ.0) THEN

    OPEN (UnPIn,file='InputFileForOpenFAST2AQWA.txt') 
      READ (UnPIn,*) ! Input file of the interface OpenFAST2AQWA that is developed by MengHAN on 10-March-2023 for performing fully-coupled analysis of floating offshore wind turbines (FOWTs).
      READ (UnPIn,*) ! --------------- OpenFAST Configuration ------------------
      READ (UnPIn,*) InputFile_AQWA       ! - Primary input file for OpenFAST
      READ (UnPIn,*) ! Flag of coupling interface. 
      READ (UnPIn,*) ! --------------- AQWA structure properties ----------- 
      READ (UnPIn,*) IndexTwrStr   ! - Index of the structure connecting to the rotor directly.
    CLOSE (UnPIn)
    CALL OpenFAST_WT(MODE,STAGE,TIME,TIMESTEP,POSITION,VELOCITY,COG,AddedPtfmFrc,AddedPtfmMom)
    OPEN (UnOuAddedPtfm,file = trim(InputFile_AQWA(1:LEN(TRIM(InputFile_AQWA))-4)//'_AddedPtfmForce_COG.dat'))
      WRITE(UnOuAddedPtfm,'(A)')  'The the platform forces acting at the platform COG due to tower base forces in the global coordinate system calculated using the DLL developed by MengHan'
      WRITE(UnOuAddedPtfm,'(A10,6(A20))') 'Time','AddedPtfmForce_COG_X','AddedPtfmForce_COG_Y','AddedPtfmForce_COG_Z','AddedPtfmForce_COG_RX','AddedPtfmForce_COG_RY','AddedPtfmForce_COG_RZ'
      WRITE(UnOuAddedPtfm,'(A10,6(A20))') '(s)','(N)','(N)','(N)','(N-m)','(N-m)','(N-m)' 
    OPEN (UnOuFORCE,file = trim(InputFile_AQWA(1:LEN(TRIM(InputFile_AQWA))-4)//'_Force_COG.dat'))
      WRITE(UnOuFORCE,'(A)')  'The the platform forces acting at the platform COG due to tower base forces in the global coordinate system calculated using the DLL developed by MengHan'
      WRITE(UnOuFORCE,'(A10,6(A20))') 'Time','Force_COG_X','Force_COG_Y','Force_COG_Z','Force_COG_RX','Force_COG_RY','Force_COG_RZ'
      WRITE(UnOuFORCE,'(A10,6(A20))') '(s)','(N)','(N)','(N)','(N-m)','(N-m)','(N-m)' 
!------------------------------------------------------------------------
! MODE=1 - On-going - calculation of forces/mass
!------------------------------------------------------------------------
!
ELSEIF (MODE.EQ.1) THEN
    IF (STAGE.EQ.1) THEN
      CALL OpenFAST_WT(MODE,STAGE,TIME,TIMESTEP,POSITION,VELOCITY,COG,AddedPtfmFrc,AddedPtfmMom) 
    ENDIF  
    FORCE(1) = AddedPtfmFrc(1)
    FORCE(2) = AddedPtfmFrc(2)
    FORCE(3) = AddedPtfmFrc(3)
    FORCE(4) = AddedPtfmMom(1)
    FORCE(5) = AddedPtfmMom(2)
    FORCE(6) = AddedPtfmMom(3)
    WRITE(UnOuAddedPtfm,'(F10.3,6(ES20.6))') TIME,(AddedPtfmFrc(I),I=1,3),(AddedPtfmMom(I),I=1,3)
    WRITE(UnOuFORCE,'(F10.3,6(ES20.6))') TIME,(FORCE(I),I=1,6)  
!
!
!!------------------------------------------------------------------------
!! MODE=99 - Termination - Output/print any summaries required/Close Files
!!           This mode is executed once at the end of the simulation
!!------------------------------------------------------------------------
!
ELSEIF (MODE.EQ.99) THEN
    CALL OpenFAST_WT(MODE,STAGE,TIME,TIMESTEP,POSITION,VELOCITY,COG,AddedPtfmFrc,AddedPtfmMom)
    CLOSE(UnOuAddedPtfm)
    CLOSE(UnOuFORCE)
!
!
!!------------------------------------------------------------------------
!! MODE# ERROR - OUTPUT ERROR MESSAGE
!!------------------------------------------------------------------------
!
ELSE	
!
ENDIF
RETURN

END SUBROUTINE OP2A_HAN







