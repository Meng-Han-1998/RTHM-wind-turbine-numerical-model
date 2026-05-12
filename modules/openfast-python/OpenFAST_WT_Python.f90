!**********************************************************************************************************************************
SUBROUTINE OpenFAST_WT_HAN(MODE_O,STAGE_O,TIME_O,TIMESTEP_O,PtfmMotAQWA_In,PtfmVelAQWA_In,PtfmCog_In,PtfmFrcAQWA_Out)
!DEC$ ATTRIBUTES C,DLLEXPORT :: OpenFAST_WT_HAN
!DEC$ ATTRIBUTES REFERENCE :: MODE_O, STAGE_O
!DEC$ ATTRIBUTES REFERENCE :: TIME_O, TIMESTEP_O
!DEC$ ATTRIBUTES REFERENCE :: PtfmMotAQWA_In, PtfmVelAQWA_In, PtfmCog_In
!DEC$ ATTRIBUTES REFERENCE :: PtfmFrcAQWA_Out   

USE FAST_Subs   ! all of the ModuleName and ModuleName_types modules are inherited from FAST_Subs
!Hm: Add the aqwa module
USE AQWAinFAST
!End of the change    
                       
IMPLICIT  NONE
   
   ! Local parameters:
REAL(DbKi),             PARAMETER     :: t_initial = 0.0_DbKi                    ! Initial time
INTEGER(IntKi),         PARAMETER     :: NumTurbines = 1                         ! Note that CalcSteady linearization analysis and WrVTK_Modes should be performed with only 1 turbine
   
   ! Other/Misc variables
TYPE(FAST_TurbineType)          ,SAVE :: Turbine(NumTurbines)                    ! Data for each turbine instance

INTEGER(IntKi)                        :: i_turb                                  ! current turbine number
INTEGER(IntKi)                        :: n_t_global                              ! simulation time step, loop counter for global (FAST) simulation
INTEGER(IntKi)                        :: ErrStat                                 ! Error status
CHARACTER(ErrMsgLen)                  :: ErrMsg                                  ! Error message

   !External input variables
INTEGER                               :: MODE_O                                  ! Status of the simulation. 0: Initilize FAST. 1: Run time marching in FAST. 99: Terminate FAST
INTEGER                               :: STAGE_O                                 ! The stage of the AQWA integration scheme. Output the FAST results at the current time step with STAGE=1
REAL                                  :: TIME_O                                  ! The current time
REAL                                  :: TIMESTEP_O                              ! The timestep size
REAL, DIMENSION (6)                   :: PtfmMotAQWA_In                          ! Position of the platform COG in the FRA
REAL, DIMENSION (6)                   :: PtfmVelAQWA_In                          ! Velocity of the platform COG in the FRA
REAL, DIMENSION (3)                   :: PtfmCog_In                              ! Position of the Centre of platform Gravity
REAL, DIMENSION (6)                   :: PtfmFrcAQWA_Out                         ! Platform forces due to tower base forces (COG)
REAL, DIMENSION (6)                   :: PtfmVelAQWA_Sq                          ! Square of Platform reference point acceleration
REAL, DIMENSION (6)             ,SAVE :: PtfmVelAQWA_Last                        ! Platform velocity at the current time step
REAL, DIMENSION (6,6)           ,SAVE :: AddBQuad_O                              ! Additional quadratic drag(N/(m/s)^2, N/(rad/s)^2, N-m(m/s)^2, N-m/(rad/s)^2)
INTEGER                         ,SAVE :: iStps                                   ! number of steps for time marching solver.= dT_AQWA/dT_FAST. In the first round, shall plus 1.

INTEGER                               :: I                                       ! Indicator for calculation
INTEGER                               :: J                                       ! Indicator for calculation
INTEGER                               :: UnPIn       = 10001                     ! I/O unit of the primary input file (InputFileForFAST2AQWA.txt). 
INTEGER                               :: UnTout      = 10002                     ! I/O unit of the primary input file (InputFileForFAST2AQWA.txt). 
INTEGER                               :: UnOuPM_COG  = 10003                     ! I/O unit of the output file containing the motions of the platform COG
INTEGER                               :: UnOuPM_RE   = 10004                     ! I/O unit of the output file containing the motions of the platform reference point
INTEGER                               :: UnOuTBF     = 10005                     ! I/O unit of the output file containing the tower base forces in the tower base coordinate system
INTEGER                               :: UnOuPF_COG  = 10006                     ! I/O unit of the output file containing the platform forces acting at the platform COG due to tower base forces in the global coordinate system
INTEGER                               :: UnOuDF_COG  = 10007                     ! I/O unit of the output file containing the platform forces acting at the platform COG due to Additional quadratic drag in the global coordinate system

   ! data for restart:
CHARACTER(1000)                       :: InputFile                               ! String to hold the intput file name
CHARACTER(1024)                       :: CheckpointRoot                          ! Rootname of the checkpoint file
CHARACTER(20)                         :: FlagArg                                 ! flag argument from command line
INTEGER(IntKi)                  ,SAVE :: Restart_step                            ! step to start on (for restart) 

IF (MODE_O.EQ.0) THEN
! First run of the DLL, open the input file
  OPEN (UnPIn,file='InputFileForOpenFAST2AQWA.txt') 
      READ (UnPIn,*) ! Input file of the interface OpenFAST2AQWA that is developed by MengHAN on 10-March-2023 for performing fully-coupled analysis of floating offshore wind turbines (FOWTs).
      READ (UnPIn,*) ! --------------- FAST Configuration ------------------
      READ (UnPIn,*) InputFile       ! - Primary input file for OpenFAST
      READ (UnPIn,*) ! - CouplingFlag		- Flag of coupling interface. False: the platform motion and velocity imported to  OpenFASTFAST WILL BE ZERO.
      READ (UnPIn,*) ! --------------- AQWA structure properties -----------
      READ (UnPIn,*) ! - Index of the structure connecting to the tower directly.
      READ (UnPIn,*) ! - Flag of whether output the platform forces
      READ (UnPIn,*) ! --------------- AddBQuad - Additional quadratic drag(N/(m/s)^2, N/(rad/s)^2, N-m(m/s)^2, N-m/(rad/s)^2) 
      DO I = 1, 6
          READ (UnPIn,*) AddBQuad_O(I,:)
      END DO
  CLOSE (UnPIn)
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! determine if this is a restart from checkpoint
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   CALL NWTC_Init() ! initialize NWTC library (set some global constants and if necessary, open console for writing)
   !ProgName = 'OpenFAST'
   !InputFile = ""
   !CheckpointRoot = ""
   !
   !CALL CheckArgs( InputFile, Flag=FlagArg, Arg2=CheckpointRoot )
   !
   !IF ( TRIM(FlagArg) == 'RESTART' ) THEN ! Restart from checkpoint file
   !   CALL FAST_RestoreFromCheckpoint_Tary(t_initial, Restart_step, Turbine, CheckpointRoot, ErrStat, ErrMsg  )
   !      CALL CheckError( ErrStat, ErrMsg, 'during restore from checkpoint'  )
   !      
   !ELSE IF ( TRIM(FlagArg) == 'VTKLIN' ) THEN ! Read checkpoint file to output linearization analysis, but don't continue time-marching
   !   CALL FAST_RestoreForVTKModeShape_Tary(t_initial, Turbine, CheckpointRoot, ErrStat, ErrMsg  )
   !      CALL CheckError( ErrStat, ErrMsg, 'during restore from checkpoint for mode shapes'  )
   !
   !   ! Note that this works only when NumTurbines==1 (we don't have files for each of the turbines...)
   !   Restart_step = Turbine(1)%p_FAST%n_TMax_m1 + 1
   !   CALL ExitThisProgram_T( Turbine(1), ErrID_None, .true., SkipRunTimeMsg = .TRUE. )
   !   
   !ELSEIF ( LEN( TRIM(FlagArg) ) > 0 ) THEN ! Any other flag, end normally
   !   CALL NormStop()
   !
   !
   !ELSE
      Restart_step = 0
      !Hm:
      CALL AQWA_Initialize()
      !End   

      DO i_turb = 1,NumTurbines
         !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         ! initialization
         !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         
         !Hm:CALL FAST_InitializeAll_T( t_initial, i_turb, Turbine(i_turb), ErrStat, ErrMsg )     ! bjj: we need to get the input files for each turbine (not necessarily the same one)
         CALL FAST_InitializeAll_T( t_initial, i_turb, Turbine(i_turb), ErrStat, ErrMsg ,InputFile)     !Hm:Passing InputFile, not read from the command line! bjj: we need to get the input files for each turbine (not necessarily the same one)
         CALL CheckError( ErrStat, ErrMsg, 'during module initialization' )
                        
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! loose coupling
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
         !...............................................................................................................................
         ! Initialization: (calculate outputs based on states at t=t_initial as well as guesses of inputs and constraint states)
         !...............................................................................................................................     
         CALL FAST_Solution0_T( Turbine(i_turb), ErrStat, ErrMsg )
         CALL CheckError( ErrStat, ErrMsg, 'during simulation initialization'  )
      
         
         !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         ! linearization (bjj: we want to call FAST_Linearize_T whenever WriteOutputToFile is called, but I'll put it at the driver level for now)
         !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            ! if we need to do linarization analysis at t=0, do it at this operating point 
         CALL FAST_Linearize_T(t_initial, 0, Turbine(i_turb), ErrStat, ErrMsg)
         CALL CheckError( ErrStat, ErrMsg  )
         
         
      END DO
   !END IF
      
iStps =  NINT(TIMESTEP_O / Turbine(1)%p_FAST%DT)    ! loop times in FAST, integer_division
!iStps = 1   
OPEN (UnTout,file = trim(InputFile(1:LEN(TRIM(InputFile))-4)//'_PtfmLoads.dat'))
   WRITE(UnTout,'(A20,F10.3)')  'TowerBsHt(m)',Turbine(1)%ED%p%TowerBsHt
   WRITE(UnTout,'(A20,F10.3)')  'PtfmRefzt(m)',Turbine(1)%ED%p%PtfmRefzt
   WRITE(UnTout,'(A20,F10.3)')  'DT(s)',Turbine(1)%p_FAST%DT
   WRITE(UnTout,'(A20,F10.3)')  'TIMESTEP_O',TIMESTEP_O
   WRITE(UnTout,'(A20,I5)')  'iStps',iStps
   WRITE(UnTout,'(6ES20.6)') (AddBQuad_O(1,I),I=1,6)
   WRITE(UnTout,'(6ES20.6)') (AddBQuad_O(2,I),I=1,6)
   WRITE(UnTout,'(6ES20.6)') (AddBQuad_O(3,I),I=1,6)
   WRITE(UnTout,'(6ES20.6)') (AddBQuad_O(4,I),I=1,6)
   WRITE(UnTout,'(6ES20.6)') (AddBQuad_O(5,I),I=1,6)
   WRITE(UnTout,'(6ES20.6)') (AddBQuad_O(6,I),I=1,6)
CLOSE (UnTout)
END IF

IF (MODE_O.EQ.1) THEN

  !...............................................................................................................................
   ! Time Stepping:
   !...............................................................................................................................         
!DO WHILE (Restart_step <= (Turbine(1)%p_FAST%n_TMax_m1))   
   !TIME_STEP_LOOP:  DO n_t_global = Restart_step, Turbine(1)%p_FAST%n_TMax_m1 
   TIME_STEP_LOOP:  DO n_t_global = Restart_step, Restart_step+iStps-1
      
         ! bjj: we have to make sure the n_TMax_m1 and n_ChkptTime are the same for all turbines or have some different logic here
      
      
         ! write checkpoint file if requested
         IF (mod(n_t_global, Turbine(1)%p_FAST%n_ChkptTime) == 0 .AND. Restart_step /= n_t_global .and. .not. Turbine(1)%m_FAST%Lin%FoundSteady) then
            CheckpointRoot = TRIM(Turbine(1)%p_FAST%OutFileRoot)//'.'//TRIM(Num2LStr(n_t_global))
         
            CALL FAST_CreateCheckpoint_Tary(t_initial, n_t_global, Turbine, CheckpointRoot, ErrStat, ErrMsg)
               IF(ErrStat >= AbortErrLev .and. AbortErrLev >= ErrID_Severe) THEN
                  ErrStat = MIN(ErrStat,ErrID_Severe) ! We don't need to stop simulation execution on this error
                  ErrMsg = TRIM(ErrMsg)//Newline//'WARNING: Checkpoint file could not be generated. Simulation continuing.'
               END IF
               CALL CheckError( ErrStat, ErrMsg  )
         END IF

      
         ! this takes data from n_t_global and gets values at n_t_global + 1
         DO i_turb = 1,NumTurbines
            !Hm:
            CALL Platform_Motion_Trans
            !End

            CALL FAST_Solution_T( t_initial, n_t_global, Turbine(i_turb), ErrStat, ErrMsg )
               CALL CheckError( ErrStat, ErrMsg  )
                                   
            
               ! if we need to do linarization analysis, do it at this operating point (which is now n_t_global + 1) 
               ! put this at the end of the loop so that we can output linearization analysis at last OP if desired
            CALL FAST_Linearize_T(t_initial, n_t_global+1, Turbine(i_turb), ErrStat, ErrMsg)
               CALL CheckError( ErrStat, ErrMsg  )
            
            !Hm:
            CALL Tower_base_Loads_Trans
            !End   
            
            !Hm:IF ( Turbine(i_turb)%m_FAST%Lin%FoundSteady) EXIT TIME_STEP_LOOP
         END DO
   END DO TIME_STEP_LOOP ! n_t_global
   !Hm:Advance FAST iStps times step in one AQWA step
   Restart_step = Restart_step+iStps
!END DO 
END IF 

IF (MODE_O.EQ.99) THEN
   DO i_turb = 1,NumTurbines
      if ( Turbine(i_turb)%p_FAST%CalcSteady .and. .not. Turbine(i_turb)%m_FAST%Lin%FoundSteady) then
         CALL CheckError( ErrID_Fatal, "Unable to find steady-state solution." )
      end if
  END DO
  
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   !  Write simulation times and stop
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   
   DO i_turb = 1,NumTurbines
      CALL ExitThisProgram_T( Turbine(i_turb), ErrID_None, i_turb==NumTurbines )
      
      CLOSE(UnOuPM_COG)
      CLOSE(UnOuPM_RE)
      CLOSE(UnOuTBF)
      CLOSE(UnOuPF_COG)
      CLOSE(UnOuDF_COG)
   END DO
END IF   

CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg,ErrLocMsg)
   ! This subroutine sets the error message and level and cleans up if the error is >= AbortErrLev
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN)           :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN)           :: Msg         ! The error message (ErrMsg)
      CHARACTER(*),   INTENT(IN), OPTIONAL :: ErrLocMsg   ! an optional message describing the location of the error

      CHARACTER(1024)                      :: SimMsg      
      integer(IntKi)                       :: i_turb2
      
      
      IF ( ErrID /= ErrID_None ) THEN
         CALL WrScr( NewLine//TRIM(Msg)//NewLine )
         
         IF ( ErrID >= AbortErrLev ) THEN
            IF (PRESENT(ErrLocMsg)) THEN
               SimMsg = ErrLocMsg
            ELSE
               SimMsg = 'at simulation time '//TRIM(Num2LStr(Turbine(1)%m_FAST%t_global))//' of '//TRIM(Num2LStr(Turbine(1)%p_FAST%TMax))//' seconds'
            END IF
            
            DO i_turb2 = 1,NumTurbines
               CALL ExitThisProgram_T( Turbine(i_turb2), ErrID, i_turb2==NumTurbines, SimMsg )
            END DO
                        
         END IF
         
      END IF


   END SUBROUTINE CheckError   
   !...............................................................................................................................
   ! -------------------------------------------------------------------------
   ! Hm:Initialize AQWA inputs and outputs:
   ! -------------------------------------------------------------------------
   SUBROUTINE AQWA_Initialize()
        ! Write notice to the user:
        WRITE (*,*) '---------------------------------------------------------------------------'
        WRITE (*,*) '------------------       OpenFAST2AQWA Interface    -----------------------'
        WRITE (*,*) '------------------------     (30-Aug-2023)    -----------------------------'
        WRITE (*,*) '---------------------------------------------------------------------------',&
                    ' OpenFASTFAST2AQWA is developed by MengHan (hanmeng@stu.ouc.edu.cn) based on',&
                    ' the F2A (YangYang) to incorporate OpenFAST with AQWA through the user_force ',&
                    ' DLL forperforming fully-coupled analysis of floating offshore wind turbines.',&
                    '---------------------------------------------------------------------------'
       DO I=1, 6
         PtfmMotAQWA(I) = 0.0
         PtfmVelAQWA(I) = 0.0
         PtfmAcceAQWA(I) = 0.0
         PtfmVelAQWA_Last(I) = 0.0
       END DO
         CouplingAQWA = .TRUE.
         PrtFASTRslts = .FALSE.
       DO I=1, 3
         PtfmFrcAQWA(I) = 0.0
         PtfmMomAQWA(I) = 0.0
       END DO 
       
       !OPEN (UnOuPM_COG,file = trim(InputFile(1:LEN(TRIM(InputFile))-4)//'_PtfmMotion_COG.dat'))
       !   WRITE(UnOuPM_COG,'(A)')  'The motions of the platform COG calculated using the DLL developed by MengHan'
       !   WRITE(UnOuPM_COG,'(A10,6(A20))') 'Time','PtfmMotion_COG_X','PtfmMotion_COG_Y','PtfmMotion_COG_Z','PtfmMotion_COG_RX','PtfmMotion_COG_RY','PtfmMotion_COG_RZ'
       !   WRITE(UnOuPM_COG,'(A10,6(A20))') '(s)','(m)','(m)','(m)','(rad)','(rad)','(rad)' 
       !   
       !OPEN (UnOuPM_RE,file = trim(InputFile(1:LEN(TRIM(InputFile))-4)//'_PtfmMotion_RE.dat'))
       !   WRITE(UnOuPM_RE,'(A)')  'The motions of the platform reference point calculated using the DLL developed by MengHan'
       !   WRITE(UnOuPM_RE,'(A10,6(A20))') 'Time','PtfmMotion_RE_X','PtfmMotion_RE_Y','PtfmMotion_RE_Z','PtfmMotion_RE_RX','PtfmMotion_RE_RY','PtfmMotion_RE_RZ'
       !   WRITE(UnOuPM_RE,'(A10,6(A20))') '(s)','(m)','(m)','(m)','(rad)','(rad)','(rad)' 
       !   
       !OPEN (UnOuTBF,file = trim(InputFile(1:LEN(TRIM(InputFile))-4)//'_TBForce_TBLC.dat'))
       !   WRITE(UnOuTBF,'(A)')  'The tower base forces in the tower base local coordinate system calculated using the DLL developed by MengHan'
       !   WRITE(UnOuTBF,'(A10,6(A20))') 'Time','TBForce_TBLC_X','TBForce_TBLC_Y','TBForce_TBLC_Z','TBForce_TBLC_RX','TBForce_TBLC_RY','TBForce_TBLC_RZ'
       !   WRITE(UnOuTBF,'(A10,6(A20))') '(s)','(N)','(N)','(N)','(N-m)','(N-m)','(N-m)' 
       !   
       !OPEN (UnOuPF_COG,file = trim(InputFile(1:LEN(TRIM(InputFile))-4)//'_PtfmForce_COG.dat'))
       !   WRITE(UnOuPF_COG,'(A)')  'The the platform forces acting at the platform COG due to tower base forces in the global coordinate system calculated using the DLL developed by MengHan'
       !   WRITE(UnOuPF_COG,'(A10,6(A20))') 'Time','PtfmForce_COG_X','PtfmForce_COG_Y','PtfmForce_COG_Z','PtfmForce_COG_RX','PtfmForce_COG_RY','PtfmForce_COG_RZ'
       !   WRITE(UnOuPF_COG,'(A10,6(A20))') '(s)','(N)','(N)','(N)','(N-m)','(N-m)','(N-m)' 
       !   
       !OPEN (UnOuDF_COG,file = trim(InputFile(1:LEN(TRIM(InputFile))-4)//'_DragForce_COG.dat'))
       !   WRITE(UnOuDF_COG,'(A)')  'The the quadratic drag forces acting at the platform COG in the global coordinate system calculated using the DLL developed by MengHan'
       !   WRITE(UnOuDF_COG,'(A10,6(A20))') 'Time','Drag_Force_COG_X','Drag_Force_COG_Y','Drag_Force_COG_Z','Drag_Force_COG_RX','Drag_Force_COG_RY','Drag_Force_COG_RZ'
       !   WRITE(UnOuDF_COG,'(A10,6(A20))') '(s)','(N)','(N)','(N)','(N-m)','(N-m)','(N-m)' 
        
   END SUBROUTINE AQWA_Initialize 
   ! -------------------------------------------------------------------------
   ! Hm:Calculation of Euler angle transformation matrix:
   ! -------------------------------------------------------------------------
   SUBROUTINE   getTransMatEuler(TransMat, Rots) !hm.该矩阵为局部到全局的旋转矩阵

      IMPLICIT  NONE

      REAL (Reki) :: TransMat (3,3)       ! Transformation matrix
      REAL (Reki) :: Rots (3)             ! Rotations
      REAL (Reki) :: cx,cy,cz             ! Cosines
      REAL (Reki) :: sx,sy,sz             ! Sines

      cx = COS(Rots(1));
      sx = SIN(Rots(1));
      cy = COS(Rots(2));
      sy = SIN(Rots(2));
      cz = COS(Rots(3));
      sz = SIN(Rots(3));
  
      TransMat(1,:) = (/ cz*cy, -sz*cx+cz*sy*sx, sz*sx+cz*sy*cx /)
      TransMat(2,:) = (/sz*cy,  cz*cx+sz*sy*sx, -cz*sx+sz*sy*cx /)
      TransMat(3,:) = (/ -sy  ,   cy*sx ,     cy*cx       /)
  
   END SUBROUTINE getTransMatEuler
   ! -------------------------------------------------------------------------
   ! Hm:Correct position vector from (Origin to CoG) TO (Origin to Reference point):
   ! -------------------------------------------------------------------------
   SUBROUTINE   Platform_Motion_Trans()

     IMPLICIT  NONE

     REAL (Reki) :: PosVec_R2C(3)          ! Position vector from the CoG to the Reference point
     REAL (Reki) :: PosVec_R2C_Traned(3)   ! Position vector transformed
     REAL (Reki) :: VelVec_R2C(3)          ! Velocity vector from the CoG to the Reference point
     REAL (Reki) :: TransMat (3,3)         ! Transformation matrix
     
     PtfmMotAQWA = PtfmMotAQWA_In
     PtfmVelAQWA = PtfmVelAQWA_In
     
     
     PosVec_R2C = PtfmCog_In
     PosVec_R2C(3) = PtfmCog_In(3) - Turbine(1)%ED%p%PtfmRefzt
     CALL getTransMatEuler(TransMat, PtfmMotAQWA(4:6))
     PosVec_R2C_Traned(1) = TransMat(1,1) * PosVec_R2C(1) + TransMat(1,2) * PosVec_R2C(2) + TransMat(1,3) * PosVec_R2C(3)
     PosVec_R2C_Traned(2) = TransMat(2,1) * PosVec_R2C(1) + TransMat(2,2) * PosVec_R2C(2) + TransMat(2,3) * PosVec_R2C(3)
     PosVec_R2C_Traned(3) = TransMat(3,1) * PosVec_R2C(1) + TransMat(3,2) * PosVec_R2C(2) + TransMat(3,3) * PosVec_R2C(3)
     PtfmMotAQWA(1:3) = PtfmMotAQWA(1:3) - PosVec_R2C_Traned
     
     CALL CrossProd(VelVec_R2C,PtfmVelAQWA(4:6),PosVec_R2C_Traned) ! Velocity due to rotations
     PtfmVelAQWA(1:3) = PtfmVelAQWA(1:3) - VelVec_R2C
     
     PtfmAcceAQWA = (PtfmVelAQWA - PtfmVelAQWA_Last)/TIMESTEP_O
     PtfmVelAQWA_Last = PtfmVelAQWA
     
     !WRITE(UnOuPM_COG,'(F10.3,6(ES20.6))') n_t_global*Turbine(1)%p_FAST%DT,(PtfmMotAQWA_In(I),I=1,6)
     !
     !WRITE(UnOuPM_RE,'(F10.3,6(ES20.6))') n_t_global*Turbine(1)%p_FAST%DT,(PtfmMotAQWA(I),I=1,6)
   
   END SUBROUTINE Platform_Motion_Trans
   
   ! -------------------------------------------------------------------------
   ! Hm:Correct position vector from (Origin to CoG) TO (Origin to Reference point):
   ! -------------------------------------------------------------------------
   SUBROUTINE   Tower_base_Loads_Trans()
     IMPLICIT  NONE

     REAL (Reki) :: PosVec_TB2C(3)         ! Position vector from tower-base to CoG
     REAL (Reki) :: PosVec_RE2C(3)         ! Position vector from reference point to CoG
     REAL (Reki) :: AddedPtfmLds_TBLC(6)   ! Platform forces acting at the CoG due to tower base forces in the tower base coordinate system
     REAL (Reki) :: AddedPtfmLds_Damp(6)   ! Platform forces acting at the CoG due to tower base forces in the tower base coordinate system
     REAL (Reki) :: MonVec_TB2C(3)         ! Moment vector from tower-base to CoG
     REAL (Reki) :: MonVec_RE2C(3)         ! Moment vector from reference point to CoG
     REAL (Reki) :: TransMat (3,3)         ! Transformation matrix
     !Convert tower-base loads to platform loads with respect to the platform reference frame
     PosVec_TB2C = PtfmCog_In
     PosVec_TB2C(3) = PtfmCog_In(3) - Turbine(1)%ED%p%TowerBsHt
     
     AddedPtfmLds_TBLC(1:3) = PtfmFrcAQWA(1:3)
     
     CALL CrossProd (MonVec_TB2C,- PosVec_TB2C,AddedPtfmLds_TBLC(1:3))!叉乘顺序

     AddedPtfmLds_TBLC(4:6) = PtfmMomAQWA + MonVec_TB2C

     ! Convert the platform loads from platform reference frame to the inertial frame coordinate system
     CALL getTransMatEuler(TransMat, PtfmMotAQWA(4:6)) ! Employ the Euler TransMat
     
     PosVec_RE2C = PtfmMotAQWA_In(1:3)-PtfmMotAQWA(1:3) ! Position vector from reference point to CoG
     
     DO I = 1, 6
        PtfmVelAQWA_Sq(I) = PtfmVelAQWA(I) * ABS(PtfmVelAQWA(I))
     END DO
     
     DO I = 1, 6
         AddedPtfmLds_Damp(I) = - DOT_PRODUCT(PtfmVelAQWA_Sq(1:6),AddBQuad_O(I,1:6))
     END DO

     CALL CrossProd (MonVec_RE2C,- PosVec_RE2C, AddedPtfmLds_Damp(1:3))
     AddedPtfmLds_Damp(4) = AddedPtfmLds_Damp(4) + MonVec_RE2C(1)
     AddedPtfmLds_Damp(5) = AddedPtfmLds_Damp(5) + MonVec_RE2C(2)
     AddedPtfmLds_Damp(6) = AddedPtfmLds_Damp(6) + MonVec_RE2C(3)
     
     
     PtfmFrcAQWA_Out(1) = DOT_PRODUCT(AddedPtfmLds_TBLC(1:3),TransMat(1,:))    ! Surge force 
     PtfmFrcAQWA_Out(2) = DOT_PRODUCT(AddedPtfmLds_TBLC(1:3),TransMat(2,:))    ! Sway  force
     PtfmFrcAQWA_Out(3) = DOT_PRODUCT(AddedPtfmLds_TBLC(1:3),TransMat(3,:))    ! Heave force
     PtfmFrcAQWA_Out(4) = DOT_PRODUCT(AddedPtfmLds_TBLC(4:6),TransMat(1,:))    ! Roll  moment
     PtfmFrcAQWA_Out(5) = DOT_PRODUCT(AddedPtfmLds_TBLC(4:6),TransMat(2,:))    ! Pitch moment
     PtfmFrcAQWA_Out(6) = DOT_PRODUCT(AddedPtfmLds_TBLC(4:6),TransMat(3,:))    ! Yaw   moment
     
     !WRITE(UnOuDF_COG,'(F10.3,6(ES20.6))') n_t_global*Turbine(1)%p_FAST%DT,(AddedPtfmLds_Damp(I),I=1,6)
     !WRITE(UnOuTBF,'(F10.3,6(ES20.6))') n_t_global*Turbine(1)%p_FAST%DT,(PtfmFrcAQWA(I),I=1,3),(PtfmMomAQWA(I),I=1,3)     
     !WRITE(UnOuPF_COG,'(F10.3,6(ES20.6))') n_t_global*Turbine(1)%p_FAST%DT,(PtfmFrcAQWA_Out(I),I=1,6)
        
     PtfmFrcAQWA_Out(1) = PtfmFrcAQWA_Out(1)  + AddedPtfmLds_Damp(1)
     PtfmFrcAQWA_Out(2) = PtfmFrcAQWA_Out(2)  + AddedPtfmLds_Damp(2)
     PtfmFrcAQWA_Out(3) = PtfmFrcAQWA_Out(3)  + AddedPtfmLds_Damp(3)
     PtfmFrcAQWA_Out(4) = PtfmFrcAQWA_Out(4) + AddedPtfmLds_Damp(4)
     PtfmFrcAQWA_Out(5) = PtfmFrcAQWA_Out(5) + AddedPtfmLds_Damp(5)
     PtfmFrcAQWA_Out(6) = PtfmFrcAQWA_Out(6) + AddedPtfmLds_Damp(6)
     
   END SUBROUTINE Tower_base_Loads_Trans
   
   SUBROUTINE CrossProd(VecResult, Vector1, Vector2)
   ! VecResult = Vector1 X Vector2 (resulting in a vector)
   IMPLICIT NONE
   ! Passed variables:
   REAL(ReKi), INTENT(OUT)         :: VecResult (3)   ! = Vector1 X Vector2 (resulting in a vector)
   REAL(ReKi), INTENT(IN )         :: Vector1   (3)
   REAL(ReKi), INTENT(IN )         :: Vector2   (3)
   VecResult(1) = Vector1(2)*Vector2(3) - Vector1(3)*Vector2(2)
   VecResult(2) = Vector1(3)*Vector2(1) - Vector1(1)*Vector2(3)
   VecResult(3) = Vector1(1)*Vector2(2) - Vector1(2)*Vector2(1)
   RETURN
   END SUBROUTINE CrossProd
! ========================================================================================
   !...............................................................................................................................
!END PROGRAM FAST
END SUBROUTINE OpenFAST_WT_HAN
!=======================================================================
