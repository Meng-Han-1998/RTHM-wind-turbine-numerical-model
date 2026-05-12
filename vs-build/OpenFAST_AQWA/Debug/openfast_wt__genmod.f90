        !COMPILER-GENERATED INTERFACE MODULE: Sun Sep 17 09:25:08 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OPENFAST_WT__genmod
          INTERFACE 
            SUBROUTINE OPENFAST_WT(MODE_O,STAGE_O,TIME_O,TIMESTEP_O,    &
     &PTFMMOTAQWA_IN,PTFMVELAQWA_IN,PTFMCOG_IN,PTFMFRCAQWA_OUT,         &
     &PTFMMOMAQWA_OUT)
              INTEGER(KIND=4), INTENT(IN) :: MODE_O
              INTEGER(KIND=4), INTENT(IN) :: STAGE_O
              REAL(KIND=4), INTENT(IN) :: TIME_O
              REAL(KIND=4), INTENT(IN) :: TIMESTEP_O
              REAL(KIND=4), INTENT(IN) :: PTFMMOTAQWA_IN(6)
              REAL(KIND=4), INTENT(IN) :: PTFMVELAQWA_IN(6)
              REAL(KIND=4), INTENT(IN) :: PTFMCOG_IN(3)
              REAL(KIND=4), INTENT(OUT) :: PTFMFRCAQWA_OUT(3)
              REAL(KIND=4), INTENT(OUT) :: PTFMMOMAQWA_OUT(3)
            END SUBROUTINE OPENFAST_WT
          END INTERFACE 
        END MODULE OPENFAST_WT__genmod
