C
C----------
C FDOEVT
C----------
C
        SUBROUTINE FDOEVT ( RECID, NEVENT, OKFLG, EXIFLG )
C
C ARGUMENTS
C
        LOGICAL * 4     OKFLG, EXIFLG
        INTEGER * 4     NEVENT
        INTEGER * 4     RECID
C
C MAIN
C
        IF ( RECID .EQ. 2 ) THEN
          CALL FINIRN
          OKFLG = .TRUE.
          EXIFLG = .FALSE.
        ELSE IF ( RECID .EQ. 8 ) THEN
          CALL FENDRN
          OKFLG = .TRUE.
          EXIFLG = .FALSE.
        ELSE
          CALL FEVUSR ( RECID, NEVENT, OKFLG, EXIFLG )
        END IF
        RETURN
        END
