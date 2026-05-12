        !COMPILER-GENERATED INTERFACE MODULE: Sun Sep 17 09:25:07 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE USER_FORCE__genmod
          INTERFACE 
            SUBROUTINE USER_FORCE(MODE,I_CONTROL,R_CONTROL,NSTRUC,TIME, &
     &TIMESTEP,STAGE,POSITION,VELOCITY,COG,FORCE,ADDMASS,ERRORFLAG)
              INTEGER(KIND=4) :: NSTRUC
              INTEGER(KIND=4) :: MODE
              INTEGER(KIND=4) :: I_CONTROL(100)
              REAL(KIND=4) :: R_CONTROL(100)
              REAL(KIND=4) :: TIME
              REAL(KIND=4) :: TIMESTEP
              INTEGER(KIND=4) :: STAGE
              REAL(KIND=4) :: POSITION(6,NSTRUC)
              REAL(KIND=4) :: VELOCITY(6,NSTRUC)
              REAL(KIND=4) :: COG(3,NSTRUC)
              REAL(KIND=4) :: FORCE(6,NSTRUC)
              REAL(KIND=4) :: ADDMASS(6,6,NSTRUC)
              INTEGER(KIND=4) :: ERRORFLAG
            END SUBROUTINE USER_FORCE
          END INTERFACE 
        END MODULE USER_FORCE__genmod
