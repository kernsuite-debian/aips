LOCAL INCLUDE 'SOLCL.INC'
C                                       Local include for SOLCL
C                                       Needs parameters from PUVD.INC.
C                                       AIPS inputs:
      INTEGER   SEQ, DISK, FREQID, BIF, EIF, SUBARR, CLVER, TYVER
      REAL      XSEQ, XDISK, XSELBN, XSELFQ, XFREQ, XBIF, XEIF,
     *   XTIME(8), XANTS(50), XSUBAR, XCLVER
      HOLLERITH XNAME(3), XCLASS(2), XSRCS(4,30), XSTOKE
C                                       Antennae with solar cal:
      LOGICAL   CALANT(MAXANT)
C                                       Source selection:
      INTEGER   SRCS(30), NSRCS
      LOGICAL   DOSWNT
C                                       Polarization selection:
      INTEGER   ISTOK
C                                       Begin and end time:
      DOUBLE PRECISION BTIME, ETIME
C                                       Main buffer:
C                                       This is sensitive to changes in
C                                       the CL table structure.
      INTEGER   BUKMRK, MBUFFI(XCLRSZ, MAXANT), NRECRD, RECRD(MAXANT)
      REAL      MBUFFR(XCLRSZ, MAXANT)
      DOUBLE PRECISION MBUFFD(XCLRSZ/2, MAXANT)
C                                       Global variables for CL file
C                                       access:
      INTEGER   CLBUFI(512), TYBUFI(512), CLLUN, TYLUN, NUMANT, NUMPOL,
     *   NUMIF, CLKOLS(MAXCLC), CLNUMV(MAXCLC), TYKOLS(MAXTYC),
     *   TYNUMV(MAXTYC), TIMCL, SOUCL, ANTCL, SUBCL,
     *   FRQCL, RE1CL, IM1CL, TS1CL, RE2CL, IM2CL, TS2CL, NCLREC
      REAL      GMMOD
C                                       Miscellaneous character data:
      CHARACTER CSRCS(30)*16
C                                       Other miscellaneous global data:
      INTEGER   FIXCNT, CNO
C                                       AIPS inputs:
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, XSRCS, XSTOKE, XSELBN,
     *   XSELFQ, XFREQ, XBIF, XEIF, XTIME, XANTS, XSUBAR, XCLVER, SEQ,
     *   DISK, FREQID, BIF, EIF, SUBARR, CLVER, TYVER
C                                       Source selection:
      COMMON /SRCSEL/ SRCS, NSRCS, DOSWNT
C                                       Main buffer:
      COMMON /SOLBUF/ MBUFFI, NRECRD, RECRD, BUKMRK
C                                       CL file globals:
      COMMON /SOLCLC/ CLBUFI, TYBUFI, GMMOD, NUMANT, NUMPOL, NUMIF,
     *   CLKOLS, CLNUMV, TYKOLS, TYNUMV, TIMCL, SOUCL, ANTCL, SUBCL,
     *   FRQCL, RE1CL, IM1CL, TS1CL,  RE2CL, IM2CL, TS2CL, NCLREC
C                                       Miscellaneous character data:
      COMMON /SOLCHR/ CSRCS
C                                       Other miscellaneous global data:
      COMMON /SOLMSC/ BTIME, ETIME, CALANT, ISTOK, FIXCNT, CNO
C
      EQUIVALENCE (MBUFFI, MBUFFR, MBUFFD)
      PARAMETER (CLLUN = 29)
      PARAMETER (TYLUN = 28)
C                                                 End SOLCL
LOCAL END
      PROGRAM SOLCL
C-----------------------------------------------------------------------
C! Apply measured nominal sensitivities to solar data.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2011
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Task SOLCL applies the nominal sensitivities of a selected subset of
C   antennae to gains in the CL file. It is assumed that the nominal
C   sensitivity of each telescope not in the selected subset is the mean
C   sensitivity of the selected antennae.
C
C   Inputs from AIPS:
C
C   INNAME             Input uv file name (name)
C   INCLASS            Input uv file name (class)
C   INSEQ              Input uv file name (sequence number)
C   INDISK             Input uv file disk number
C   SOURCES            Source list
C   STOKES             Polarizations to process
C   SELBAND            Bandwidth to process
C   SELFREQ            Frequency to process
C   FREQID             Frequency ID to process
C   BIF                Lowest numbered IF to process
C   EIF                Highest numbered IF to process
C   TIMERANG           Time range to process
C   ANTENNAS           Antennas to correct
C   SUBARRAY           Subarray
C   GAINVER            CL table version to update
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   BUFFER(512), IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SOLCL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      PARAMETER (PRGM = 'SOLCL ')
C-----------------------------------------------------------------------
C                                       Get input parameters and open
C                                       necessary files:
      CALL SOLINI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Scan CL file and apply
C                                       corrections to selected records:
      CALL SOLCOR (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Update history file:
      CALL SOLHIS
C                                       Close down:
 990  CALL DIE (IRET, BUFFER)
 999  STOP
      END
      SUBROUTINE SOLINI (PRGM, IRET)
C-----------------------------------------------------------------------
C   Get input parameters for SOLCL.
C
C   Input:
C     PRGM     C*6      Program name
C
C   Output:
C     IRET     I        Exit code: 0 => OK
C                                  1 => error obtaining inputs
C                                  2 => catalogue error
C                                  3 => input file problem
C                                  4 => other error
C
C   Outputs in common:
C     TSKNAM   C*6      Task name
C     RLSNAM   C*7      Release date
C
C     CNO      I        Catalogue number of data file
C     DISK     I        Disk number for AIPS data
C     CLVER    I        Version number of CL file
C     TYVER    I        Version number of TY file
C
C     FREQID   I        Selected frequency ID
C     BIF      I        Lowest numbered IF to correct
C     EIF      I        Highest numbered IF to correct
C
C     SUBARR   I        Subarray number
C
C     ISTOK    I        Polarizations to correct
C
C     CALANT   L(*)     CALANT(I) is .TRUE. if antenna I has good solar
C                       cal and .FALSE. otherwise
C
C     CSRCS    C(30)*8  List of selected/deselected sources
C     SRCS     I(30)    Source numbers corresponding to entries in CSRCS
C     NSRCS    I        Number of entries in SRCS
C     DOSWNT   L        .TRUE. if sources in CSRCS are selected
C
C     BTIME    D        Start time
C     ETIME    D        End time
C
C     NUMPOL   I        Number of polarizations (1 or 2) from catalogue
C     NUMIF    I        Number of IFs in uv file
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET

      INTEGER   BUFFER(1024), I, IERR, LUN, NANT, NPARM, IROUND, HVER
      LOGICAL   MATCH
      DOUBLE PRECISION SELFRQ
      CHARACTER CLASS*6, NAME*12, STAT*4, STOKES*2, UTYPE*2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SOLCL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      PARAMETER (NPARM = 193)
      PARAMETER (LUN = 30)
C      DATA CALANT / MAXANT * .FALSE. /
C-----------------------------------------------------------------------
C                                       Initialize for AIPS etc.:
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
      IRET = 0
      DO 5 I = 1,MAXANT
         CALANT(I) = .FALSE.
 5       CONTINUE
C                                       Get AIPS parameters:
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 1
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS:
      IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Crunch input parameters:
      SEQ = IROUND (XSEQ)
      DISK = IROUND (XDISK)
      SUBARR = IROUND (XSUBAR)
      IF (SUBARR.LE.0) SUBARR = 1
      CLVER = IROUND (XCLVER)
      TYVER = 1
      MSGTXT = 'Using TY version 1'
      CALL MSGWRT (4)
      FREQID = IROUND (XFREQ)
      SELFRQ = DBLE (XSELFQ)
C                                       Convert characters:
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL H2CHR (2, 1, XSTOKE, STOKES)
      DO 10 I = 1, 30
         CALL H2CHR (16, 1, XSRCS(1, I), CSRCS(I))
 10      CONTINUE
C                                       Find data file and read header:
      CNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISK, CNO, NAME, CLASS, SEQ, UTYPE, NLUSER,
     *   STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         IF (IERR.EQ.5) THEN
            WRITE (MSGTXT,1010) NAME, CLASS, SEQ, DISK, NLUSER
         ELSE
            WRITE (MSGTXT,1011) IERR, NAME, CLASS, SEQ, DISK, NLUSER
            END IF
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK, CNO, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1012) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
C                                       Get uv header info:
      CALL UVPGET (IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1013) IERR
         GO TO 990
         END IF
C                                       Check sort order:
      IF (ISORT(1:2).NE.'TB') THEN
         IRET = 3
         WRITE (MSGTXT,1014) ISORT
         GO TO 990
         END IF
C                                       Set Stokes type:
      IF (CATBLK(KINAX+JLOCS).GT.1) THEN
         NUMPOL = 2
      ELSE
         NUMPOL = 1
         END IF
      IF (STOKES.EQ.'R ') THEN
         ISTOK = 1
      ELSE IF (STOKES.EQ.'L ') THEN
         ISTOK = 2
      ELSE
         ISTOK = 0
         END IF
C                                       Set IF range:
      IF (JLOCIF.GE.0) THEN
         NUMIF = CATBLK(KINAX+JLOCIF)
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (BIF.GT.EIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         NUMIF = 1
         BIF = 1
         EIF = 1
         END IF
C                                       Get selected frequency ID:
      CALL FQMATC (DISK, CNO, CATBLK, LUN, XSELBN, SELFRQ, MATCH,
     *   FREQID, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1015) IERR
         GO TO 990
         END IF
      IF (.NOT.MATCH) THEN
         IRET = 4
         WRITE (MSGTXT,1016)
         GO TO 990
         END IF
C                                       Process antenna list:
      NANT = 0
      DO 100 I = 1,50
         IF (XANTS(I).LT.0) THEN
            IRET = 4
            WRITE (MSGTXT,1017)
            GO TO 990
         ELSE IF (XANTS(I).GT.0) THEN
            CALANT(IROUND (XANTS(I))) = .TRUE.
            NANT = NANT + 1
            END IF
 100     CONTINUE
C                                       Check for all blank (all
C                                       telescopes have solar cal):
      IF (NANT.EQ.0) THEN
         WRITE (MSGTXT,1100)
         CALL MSGWRT (6)
         DO 105 I = 1,MAXANT
            CALANT(I) = .TRUE.
 105        CONTINUE
         END IF
C                                       Get source numbers:
      CALL FNDSOU (DISK, CNO, CSRCS, BUFFER, NSRCS, DOSWNT, SRCS, IERR)
      IF (IERR.NE.0) THEN
         IRET = 3
         WRITE (MSGTXT,1105) IERR
         GO TO 990
         END IF
C                                       Time range:
      BTIME = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0 * 60.0) +
     *   XTIME(4) / (24.0 * 60.0 * 60.0)
      ETIME = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0 * 60.0) +
     *   XTIME(8) / (24.0 * 60.0 * 60.0)
      IF (ETIME.LE.BTIME) ETIME = 99999.9
C                                       Check CL version numbers
      CALL FNDEXT ('CL', CATBLK, HVER)
      IF (HVER.EQ.1) THEN
         CALL TABCOP ('CL', 1, 2, 23, 24, DISK, DISK, CNO, CNO, CATBLK,
     *      MBUFFR, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR COPYING CL VERSION 1 TO 2'
            GO TO 990
            END IF
         HVER = 2
         CLVER = MAX (CLVER, 2)
         END IF
      IF (CLVER.LE.0) CLVER = HVER
C                                       Disallow CL table 1:
      IF (CLVER.EQ.1) THEN
         IRET = 4
         MSGTXT = 'MODIFYING CL TABLE VERSION 1 IS NOT ALLOWED'
         GO TO 990
         END IF
      IF (CLVER.GT.HVER) THEN
         IRET = 4
         WRITE (MSGTXT, 1106) CLVER, HVER
         GO TO 990
         END IF
      GO TO 999
C                                       Exception handler:
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOLINI: ERROR',I3,' READING INPUT PARAMETERS')
 1010 FORMAT ('CAN NOT FIND ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' USID=',I5)
 1011 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' USID=',I5)
 1012 FORMAT ('SOLINI: ERROR',I3,' READING CATALOG HEADER')
 1013 FORMAT ('SOLINI: ERROR',I3,' GETTING HEADER POINTERS')
 1014 FORMAT ('INPUT VISIBILITIES MISSORTED, SORTED = ',A2,
     *   ' (SHOULD BE TB)')
 1015 FORMAT ('SOLINI: ERROR',I3,' GETTING FREQUENCY ID')
 1016 FORMAT ('NO MATCH TO SELBAND/SELFREQ --- CHECK ADVERBS')
 1017 FORMAT ('YOU CAN NOT DESELECT ANTENNAE IN THIS TASK')
 1100 FORMAT ('WARNING: all antennae assumed to have valid cal.')
 1105 FORMAT ('SOLINI: ERROR',I3,' READING SOURCE TABLE')
 1106 FORMAT ('GAINVER MUST ALREADY EXIST.  VERSION',I3,' > MAX',I3)
      END
      SUBROUTINE SOLCOR (IRET)
C-----------------------------------------------------------------------
C   Scan through the CL table for selected records and make corrections.
C
C   Output:
C     IRET        I          Error status: 0 => OK
C                                          1 => error opening CL table
C                                          2 => CL table I/O error
C                                          3 => error closing CL table
C                                          4 => other problem
C
C   Inputs in common:
C     DISK        I          Disk number used
C     CNO         I          AIPS catalogue number
C     CLVER       I          CL file version number
C
C     SRCS        I(30)      List of selected/deselected source numbers
C     NSRCS       I          Number of sources in SRCS
C     DOSWNT      L          .TRUE. if sources in SRCS are selected,
C                            .FALSE. if sources in SRCS are deselected
C
C     ISTOK       I          Polarizations to process: 0 => all
C                                                      1 => first only
C                                                      2 => second only
C
C     FREQID      I          Frequency ID to process: 0 => all
C     BIF         I          First IF to process
C     EIF         I          Last IF to process
C
C     XTIME       R(8)       Time range to process
C
C     CALANT      L(*)       CALANT(I) is .TRUE. only if antenna I is
C                            deemed to have valid solar cal
C
C     SUBARR      I          Subarray to process
C
C   Output in common:
C     FIXCNT      I          Number of records modified
C-----------------------------------------------------------------------
      INTEGER   IRET

      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, IERR, J, N(MAXIF)
      LOGICAL   DONE, WARNED
      REAL      AV(MAXIF), SUM(MAXIF)
      INCLUDE 'SOLCL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Open the CL table:
      CALL SOPNCL (IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Check polarization:
      IF ((ISTOK.EQ.2) .AND. (NUMPOL.EQ.1)) THEN
         IRET = 4
         WRITE (MSGTXT,1001)
         GO TO 990
         END IF
C                                       Scan the CL table:
      FIXCNT = 0
      WARNED = .FALSE.
      DONE = .FALSE.
      CALL SRDCL (DONE, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1002) IERR
         GO TO 990
         END IF
 100  IF (.NOT.DONE) THEN
C                                       Do first polarization:
         IF ((ISTOK.EQ.0) .OR. (ISTOK.EQ.1)) THEN
C                                       Get average nominal sensitivity:
            DO 200 I = BIF, EIF
               SUM(I) = 0.0
               N(I) = 0
 200           CONTINUE
            DO 220 I = 1, NRECRD
               IF (CALANT(MBUFFI(ANTCL, I))) THEN
                  DO 210 J = BIF, EIF
                     IF (MBUFFR(TS1CL+J-1, I).NE.FBLANK) THEN
                        SUM(J) = SUM(J) + MBUFFR(TS1CL+J-1, I)
                        N(J) = N(J) + 1
                        END IF
 210                 CONTINUE
                  END IF
 220           CONTINUE
            DO 230 I = BIF, EIF
               IF (N(I).EQ.0) THEN
                  AV(I) = FBLANK
               ELSE
                  AV(I) = SUM(I) / REAL (N(I))
                  END IF
 230           CONTINUE
C                                       Correct data:
            DO 260 I = 1, NRECRD
               IF (CALANT(MBUFFI(ANTCL, I))) THEN
                  DO 240 J = BIF, EIF
                     IF (MBUFFR(TS1CL+J-1, I).NE.FBLANK) THEN
                        IF (MBUFFR(RE1CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(RE1CL+J-1, I) =
     *                     MBUFFR(RE1CL+J-1, I) *
     *                     SQRT(MBUFFR(TS1CL+J-1, I))
                        IF (MBUFFR(IM1CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(IM1CL+J-1, I) =
     *                     MBUFFR(IM1CL+J-1, I) *
     *                     SQRT(MBUFFR(TS1CL+J-1, I))
                     ELSE
                        IF (MBUFFR(RE1CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(RE1CL+J-1, I) = FBLANK
                        IF (MBUFFR(IM1CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(IM1CL+J-1, I) = FBLANK
                        IF (.NOT.WARNED) THEN
                           WRITE (MSGTXT,1230)
                           CALL MSGWRT (6)
                           WARNED = .TRUE.
                           END IF
                        END IF
 240                 CONTINUE
               ELSE
                  DO 250 J = BIF, EIF
                     IF (AV(J).NE.FBLANK) THEN
                        IF (MBUFFR(RE1CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(RE1CL+J-1, I) =
     *                     MBUFFR(RE1CL+J-1, I) * SQRT(AV(J))
                        IF (MBUFFR(IM1CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(IM1CL+J-1, I) =
     *                     MBUFFR(IM1CL+J-1, I) * SQRT(AV(J))
                     ELSE
                        IF (MBUFFR(RE1CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(RE1CL+J-1, I) = FBLANK
                        IF (MBUFFR(IM1CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(IM1CL+J-1, I) = FBLANK
                        IF (.NOT.WARNED) THEN
                           WRITE (MSGTXT,1230)
                           CALL MSGWRT (6)
                           WARNED = .TRUE.
                           END IF
                        END IF
 250                 CONTINUE
                  END IF
 260           CONTINUE
            END IF
C                                       Do second polarization:
         IF ((ISTOK.EQ.0) .OR. (ISTOK.EQ.2)) THEN
C                                       Get average nominal sensitivity:
            DO 300 I = BIF, EIF
               SUM(I) = 0.0
               N(I) = 0
 300           CONTINUE
            DO 320 I = 1, NRECRD
               IF (CALANT(MBUFFI(ANTCL, I))) THEN
                  DO 310 J = BIF, EIF
                     IF (MBUFFR(TS2CL+J-1, I).NE.FBLANK) THEN
                        SUM(J) = SUM(J) + MBUFFR(TS2CL+J-1, I)
                        N(J) = N(J) + 1
                        END IF
 310                 CONTINUE
                  END IF
 320           CONTINUE
            DO 330 I = BIF, EIF
               IF (N(I).EQ.0) THEN
                  AV(I) = FBLANK
               ELSE
                  AV(I) = SUM(I) / REAL (N(I))
                  END IF
 330           CONTINUE
C                                       Correct data:
            DO 360 I = 1, NRECRD
               IF (CALANT(MBUFFI(ANTCL, I))) THEN
                  DO 340 J = BIF, EIF
                     IF (MBUFFR(TS2CL+J-1, I).NE.FBLANK) THEN
                        IF (MBUFFR(RE2CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(RE2CL+J-1, I) =
     *                     MBUFFR(RE2CL+J-1, I) *
     *                     SQRT(MBUFFR(TS2CL+J-1, I))
                        IF (MBUFFR(IM2CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(IM2CL+J-1, I) =
     *                     MBUFFR(IM2CL+J-1, I) *
     *                     SQRT(MBUFFR(TS2CL+J-1, I))
                     ELSE
                        IF (MBUFFR(RE2CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(RE2CL+J-1, I) = FBLANK
                        IF (MBUFFR(IM2CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(IM2CL+J-1, I) = FBLANK
                        IF (.NOT.WARNED) THEN
                           WRITE (MSGTXT,1230)
                           CALL MSGWRT (6)
                           WARNED = .TRUE.
                           END IF
                        END IF
 340                 CONTINUE
               ELSE
                  DO 350 J = BIF, EIF
                     IF (AV(J).NE.FBLANK) THEN
                        IF (MBUFFR(RE2CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(RE2CL+J-1, I) =
     *                     MBUFFR(RE2CL+J-1, I) * SQRT(AV(J))
                        IF (MBUFFR(IM2CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(IM2CL+J-1, I) =
     *                     MBUFFR(IM2CL+J-1, I) * SQRT(AV(J))
                     ELSE
                        IF (MBUFFR(RE2CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(RE2CL+J-1, I) = FBLANK
                        IF (MBUFFR(IM2CL+J-1, I).NE.FBLANK)
     *                     MBUFFR(IM2CL+J-1, I) = FBLANK
                        IF (.NOT.WARNED) THEN
                           WRITE (MSGTXT,1230)
                           CALL MSGWRT (6)
                           WARNED = .TRUE.
                           END IF
                        END IF
 350                 CONTINUE
                  END IF
 360           CONTINUE
            END IF
C                                       Write back corrected records:
         CALL SWRCL (IERR)
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1360) IERR
            GO TO 990
            END IF
C                                       Add number changed to running
C                                       total:
         FIXCNT = FIXCNT + NRECRD
C                                       Get next batch of records:
         CALL SRDCL (DONE, IERR)
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1002) IERR
            GO TO 990
            END IF
         GO TO 100
         END IF
C                                       End of main loop.
C                                       Close CL table:
      CALL SOCLCL (IERR)
      IF (IERR.NE.0) THEN
         IRET = 3
         WRITE (MSGTXT,1361) IERR
         GO TO 990
         END IF
C                                       Tell user how many records were
C                                       changed:
      WRITE (MSGTXT,1362) FIXCNT
      CALL MSGWRT (4)
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOLCOR: ERROR',I3,' OPENING CL TABLE')
 1001 FORMAT ('REQUESTED POLARIZATION IS NOT PRESENT')
 1002 FORMAT ('SOLCOR: ERROR',I3,' READING CL TABLE')
 1230 FORMAT ('SOME BAD GAIN SOLUTIONS FOUND --- CHECK OUTPUT TABLE')
 1360 FORMAT ('SOLCOR: ERROR',I3,' READING CL TABLE')
 1361 FORMAT ('SOLCOR: ERROR',I3,' CLOSING CL TABLE')
 1362 FORMAT ('Modified',I6,' CL table records')
      END
      SUBROUTINE SOPNCL (IRET)
C-----------------------------------------------------------------------
C   Open the requested CL table and set up column pointers.
C
C   Output:
C     IRET      I        Error status: 0 => OK
C                                      1 => Can't open CL table
C
C   Input in common:
C     DISK      I        AIPS disk number
C     CNO       I        AIPS catalogue number
C     CLVER     I        CL file version number
C     CATBLK    I(*)     uv file header
C
C   Output in common:
C     CLBUFI    I(512)   CL file buffer
C     CLKOLS    I(*)     Column pointer array
C     CLNUMV    I(*)     Number of elements in each column
C
C     BUKMRK    I        Next CL file record to read (= 1)
C
C     NUMANT    I        Number of antennae in table
C     NUMPOL    I        Number of polarizations in table
C     NUMIF     I        Number of IFs in table
C
C     GMMOD     I        Mean gain modulus
C
C     NCLREC    I        Number of records in CL table
C
C     TIMCL     I        Pointer to time entry
C     SOUCL     I        Pointer to source entry
C     ANTCL     I        Pointer to antenna entry
C     SUBCL     I        Pointer to subarray entry
C     FRQCL     I        Pointer to frequency ID entry
C     RE1CL     I        Pointer to first real gain entry for poln 1
C     IM1CL     I        Pointer to first imag. gain entry for poln 1
C     TS1CL     I        Pointer to first sensitivity entry for poln 1
C     RE2CL     I        Pointer to first real gain entry for poln 2
C     IM2CL     I        Pointer to first imag. gain entry for poln 2
C     TS2CL     I        Pointer to first sensitivity entry for poln 2
C-----------------------------------------------------------------------
      INTEGER   IRET

      INTEGER   ICLRNO, ITYRNO, IERR, NTERM, KEY(2,2),
     *   KEYSUB(2,2)
      REAL      FKEY(2,2)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'INCS:PTYTAB.INC'
      INCLUDE 'SOLCL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Reformat table if necessary:
      CALL CLREFM (DISK, CNO, CLVER, CATBLK, CLLUN, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Sort tables to time-antenna
C                                       order
      KEY(1,1) = CLDTIM
      KEY(2,1) = CLDTIM
      KEY(1,2) = CLIANT
      KEY(2,2) = CLIANT
      CALL TABSRT (DISK, CNO, 'CL', CLVER, CLVER, KEY, KEYSUB, FKEY,
     *    CLBUFI, CATBLK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        TY table
      KEY(1,1) = TYRTIM
      KEY(2,1) = TYRTIM
      KEY(1,2) = TYIANT
      KEY(2,2) = TYIANT
      CALL TABSRT (DISK, CNO, 'TY', TYVER, TYVER, KEY, KEYSUB, FKEY,
     *   CLBUFI, CATBLK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open the CL file:
      CALL CALINI ('WRIT', CLBUFI, DISK, CNO, CLVER, CATBLK, CLLUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1001) 'CL', IERR
         GO TO 990
         END IF
C                                       Open the TY file:
      CALL TYINI ('READ', TYBUFI, DISK, CNO, TYVER, CATBLK, TYLUN,
     *   ITYRNO, TYKOLS, TYNUMV, NUMPOL, NUMIF, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1001) 'TY', IERR
         GO TO 990
         END IF
      BUKMRK = 1
      NCLREC = CLBUFI(5)
C                                       Check that there are the same
C                                       number of rows in both tables.
      IF (CLBUFI(5).NE.TYBUFI(5)) THEN
         IRET = 5
         MSGTXT = 'CL AND TY TABLES ARE DIFFERENT SIZES'
         GO TO 990
         END IF
C                                       Set column indices:
      TIMCL = CLKOLS(CLDTIM)
      SOUCL = CLKOLS(CLISID)
      ANTCL = CLKOLS(CLIANT)
      SUBCL = CLKOLS(CLISUB)
      FRQCL = CLKOLS(CLIFQI)
      RE1CL = CLKOLS(CLRRE1)
      IM1CL = CLKOLS(CLRIM1)
      TS1CL = CLKOLS(CLIRF2) + NUMIF
      RE2CL = CLKOLS(CLRRE2)
      IM2CL = CLKOLS(CLRIM2)
      TS2CL = TS1CL + NUMIF
      IRET = 0
      GO TO 999
C                                       Exception handler:
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOPNCL: ERROR',I4,' REFORMATTING CL TABLE')
 1001 FORMAT ('SOPNCL: ERROR',I4,' INITIALIZING ',A,' TABLE')
      END
      SUBROUTINE SRDCL (DONE, IRET)
C-----------------------------------------------------------------------
C   Read in all CL/TY records for the next selected time:
C
C   Outputs:
C     DONE       L            .TRUE. if no more data to read
C     IRET       I            Error status: 0 => No error
C                                           1 => read problem
C
C   Inputs in common:
C     CLBUFI     I(512)       CL table I/O control
C     TIMCL      I            Pointer to time column in CL table
C     SOUCL      I            Pointer to source column in CL table
C     FRQCL      I            Pointer to frequency ID in CL table
C
C     SRCS       I(30)        List of selected/deselected sources
C     NSRCS      I            Number of entries in SRCS
C     DOSWNT     L            .TRUE. if sources are selected
C
C     BTIME      D            Start time
C     ETIME      D            End time
C
C     FREQID     I            Requested frequency ID
C
C   I/O in common:
C     BUKMRK     I            Next record to read
C     MBUFFI     I(LENREC, *) Stores CL records for a given time
C     MBUFFR     R(*, *)      Stores CL records for a given time
C                             (equivalenced to MBUFFI)
C     MBUFFD     D(*, *)      Stores CL records for a given time
C                             (equivalenced to MBUFFI)
C     RECRD      I(*)         Maps entries in MBUFFI to CL table record
C                             numbers
C     NRECRD     I            Number of entries in MBUFFI/RECRD
C-----------------------------------------------------------------------
      INTEGER    IRET
      LOGICAL    DONE

      INCLUDE 'INCS:PUVD.INC'
      INTEGER    IERR, TYSID, TYANT, TYSUBA, TYFIQ, I, IP, ITYRNO
      REAL       TSYS(2,MAXIF), TANT(2,MAXIF), TYTIME, TYTIMI
      DOUBLE PRECISION TENTH
      LOGICAL    WNTSRC
      INCLUDE 'SOLCL.INC'
      INCLUDE 'DMSG.INC'
      INCLUDE 'DDCH.INC'
      PARAMETER (TENTH = 0.1 / (24.0 * 60.0 * 60.0))
C-----------------------------------------------------------------------
      DONE = .FALSE.
      NRECRD = 0
C                                       Read the next unflagged record:
 10   IF (BUKMRK.GT.NCLREC) THEN
         DONE = .TRUE.
         IRET = 0
         GO TO 999
         END IF
      CALL TABIO ('READ', 0, BUKMRK, MBUFFI(1, 1), CLBUFI, IERR)
      BUKMRK = BUKMRK + 1
      IF (IERR.EQ.-1) THEN
         GO TO 10
      ELSE IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Loop over records before
C                                       requested time:
 100  IF (MBUFFD(TIMCL, NRECRD+1).LT.BTIME) THEN
 110     IF (BUKMRK.GT.NCLREC) THEN
            DONE = .TRUE.
            IRET = 0
            GO TO 999
            END IF
         CALL TABIO('READ', 0, BUKMRK, MBUFFI(1, 1), CLBUFI, IERR)
         BUKMRK = BUKMRK + 1
         IF (IERR.EQ.-1) THEN
            GO TO 110
         ELSE IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
         GO TO 100
         END IF
C                                       Skip records that do not meet
C                                       selection criteria:
 200  IF ((.NOT.WNTSRC(MBUFFI(SOUCL, 1))) .OR.
     *   ((MBUFFI(SUBCL,1).NE.SUBARR) .AND. (MBUFFI(SUBCL,1).GT.0)) .OR.
     *   ((FREQID.GT.0) .AND. (MBUFFI(FRQCL,1).NE.FREQID) .AND.
     *   (MBUFFI(FRQCL,1).GT.0))) THEN
 210     IF (BUKMRK.GT.NCLREC) THEN
            DONE = .TRUE.
            IRET = 0
            GO TO 999
            END IF
         CALL TABIO ('READ', 0, BUKMRK, MBUFFI(1, 1), CLBUFI, IERR)
         BUKMRK = BUKMRK + 1
         IF (IERR.EQ.-1) THEN
            GO TO 210
         ELSE IF (IERR.NE.0) THEN
           IRET = 1
           WRITE (MSGTXT,1000) IERR
           GO TO 990
           END IF
        GO TO 200
        END IF
C                                       Check if past end time:
      IF (MBUFFD(TIMCL, 1).GT.ETIME) THEN
         DONE = .TRUE.
         IRET = 0
         GO TO 999
         END IF
C                                       TY table entry
      ITYRNO = BUKMRK - 1
      CALL TABTY ('READ', TYBUFI, ITYRNO, TYKOLS, TYNUMV, NUMPOL,
     *   NUMIF, TYTIME, TYTIMI, TYSID, TYANT, TYSUBA, TYFIQ, TSYS,
     *   TANT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check for matching time and
C                                       antenna.
      IP = 1
      IF ((ABS (MBUFFD(TIMCL,IP)-TYTIME).GT.1.2D-5) .OR.
     *   (MBUFFI(ANTCL,IP).NE.TYANT)) THEN
         IRET = 1
         MSGTXT = 'CL AND TY TABLE DO NOT MATCH'
         GO TO 990
      END IF
C                                       Copy TSYS, use 1 if blanked.
      DO 290 I = 1,NUMIF
         IF (TSYS(1,I).NE.FBLANK) THEN
            MBUFFR(TS1CL+I-1,IP) = TSYS(1,I)
         ELSE
            MBUFFR(TS1CL+I-1,IP) = 1.0
            END IF
         IF (TSYS(1,I).NE.FBLANK) THEN
            MBUFFR(TS2CL+I-1,IP) = TSYS(2,I)
         ELSE
            MBUFFR(TS2CL+I-1,IP) = 1.0
            END IF
 290      CONTINUE
C                                       Accumulate records until time
C                                       changes:
 300  IF (ABS (MBUFFD(TIMCL, NRECRD+1) - MBUFFD(TIMCL, 1)).LT.TENTH)
     *   THEN
         NRECRD = NRECRD + 1
         RECRD(NRECRD) = BUKMRK - 1
 310     IF (BUKMRK.GT.NCLREC) THEN
            IRET = 0
            GO TO 999
            END IF
         CALL TABIO ('READ', 0, BUKMRK, MBUFFI(1, NRECRD+1), CLBUFI,
     *      IERR)
            BUKMRK = BUKMRK + 1
            IF (IERR.EQ.-1) THEN
               GO TO 310
            ELSE IF (IERR.NE.0) THEN
               IRET = 1
               WRITE (MSGTXT,1000) IERR
               GO TO 990
               END IF
C                                       TY table entry
            ITYRNO = BUKMRK - 1
            CALL TABTY ('READ', TYBUFI, ITYRNO, TYKOLS, TYNUMV, NUMPOL,
     *         NUMIF, TYTIME, TYTIMI, TYSID, TYANT, TYSUBA, TYFIQ, TSYS,
     *         TANT, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Check for matching time and
C                                       antenna.
            IP = NRECRD + 1
            IF ((ABS (MBUFFD(TIMCL,IP)-TYTIME).GT.1.2D-5) .OR.
     *         (MBUFFI(ANTCL,IP).NE.TYANT)) THEN
               IRET = 1
               MSGTXT = 'CL AND TY TABLE DO NOT MATCH'
               GO TO 990
               END IF
C                                       Copy TSYS
            DO 390 I = 1,NUMIF
               IF (TSYS(1,I).NE.FBLANK) THEN
                  MBUFFR(TS1CL+I-1,IP) = TSYS(1,I)
               ELSE
                  MBUFFR(TS1CL+I-1,IP) = 1.0
                  END IF
               IF (TSYS(1,I).NE.FBLANK) THEN
                  MBUFFR(TS2CL+I-1,IP) = TSYS(2,I)
               ELSE
                  MBUFFR(TS2CL+I-1,IP) = 1.0
                  END IF
 390           CONTINUE
         IF ((.NOT.WNTSRC(MBUFFI(SOUCL, NRECRD+1))) .OR.
     *      ((MBUFFI(SUBCL,NRECRD+1).NE.SUBARR) .AND.
     *      (MBUFFI(SUBCL,NRECRD+1).GT.0)) .OR.
     *      ((FREQID.GT.0) .AND. (MBUFFI(FRQCL,NRECRD+1).NE.FREQID)
     *      .AND. (MBUFFI(FRQCL,NRECRD+1).GT.0)))
     *      GO TO 310
         GO TO 300
         END IF
C                                       We will want to re-read the last
C                                       record next time round:
      BUKMRK = BUKMRK - 1
      IRET = 0
      GO TO 999
C                                       Exception handler
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SRDCL: ERROR',I2,' READING CL TABLE')
      END
      SUBROUTINE SWRCL (IRET)
C-----------------------------------------------------------------------
C   Write modified CL records back to CL table.
C
C   Output:
C     IRET        I                Return code: 0 => OK
C                                               1 => TABIO error
C
C   Inputs in common:
C     CLBUFI      I(512)           CL table I/O control block
C     MBUFFI      I(LENREC, *)     Modified records
C     RECRD       I(*)             Maps MBUFFI entries to record numbers
C     NRECRD      I                Number of entries in MBUFFI/RECRD
C-----------------------------------------------------------------------
      INTEGER   IRET

      INTEGER   I, IERR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SOLCL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      DO 10 I = 1, NRECRD
         CALL TABIO ('WRIT', 0, RECRD(I), MBUFFI(1, I), CLBUFI, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
 10      CONTINUE
      IRET = 0
      GO TO 999
C                                       Exception handler:
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SWRCL: ERROR',I2,' WRITING TO CL TABLE')
      END
      SUBROUTINE SOCLCL (IRET)
C-----------------------------------------------------------------------
C   Close CL file
C
C   Output:
C     IRET         I         Error status: 0 => OK
C                                          1 => failed to close
C
C   Input in common:
C     CLBUFI       I         CL table I/O control block
C-----------------------------------------------------------------------
      INTEGER   IRET

      INTEGER   BUFFER(512), IERR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SOLCL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL TABIO ('CLOS', 0, 0, BUFFER, CLBUFI, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) 'CL', IERR
         GO TO 990
         END IF
C                                       TY table
      CALL TABIO ('CLOS', 0, 0, BUFFER, TYBUFI, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) 'TY', IERR
         GO TO 990
         END IF
C
      IRET = 0
      GO TO 999
C                                       Exception handler:
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOCLCL: ERROR',I4,' CLOSING ',A,' TABLE')
      END
      SUBROUTINE SOLHIS
C-----------------------------------------------------------------------
C   Update history file.
C
C   Inputs in common:
C     TSKNAM        C*(*)     Task name
C     RLSNAM        C*(*)     AIPS release
C
C     CLVER         I         Version number of CL table
C     DISK          I         Disk number for uv data file
C
C     SUBARR        I         Subarray number
C     XANTS         R(50)     List of reference antenna
C
C     CSRCS         C(30)*8   List of selected/deselected sources
C     NSRCS         I         Number of sources in CSRCS (zero if all
C                             selected)
C     DOSWNT        L         .TRUE. if sources in SRCS selected
C
C     XTIME         R(8)      Selected timerange
C
C     XSTOKE        H         Polarizations corrected
C
C     FREQID        I         Frequency ID corrected
C     BIF           I         Lowest numbered IF corrected
C     EIF           I         Highest numbered IF corrected
C-----------------------------------------------------------------------
      INTEGER   BUFFER(256), DATE(3), HISLUN, I, IERR, J, LIMIT, LIMIT2,
     *   TIME(3), ANTS(50), NANTS
      CHARACTER CDATE*12, CTIME*8, HILINE*72, KEYWRD*10, STOKES*2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SOLCL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      PARAMETER (HISLUN = 27)
C-----------------------------------------------------------------------
      CALL HIINIT (3)
C                                       Open history file:
      CALL HIOPEN (HISLUN, DISK, FCNO(NCFILE), BUFFER, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       Task message:
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (HILINE,1001) TSKNAM, RLSNAM, CDATE, CTIME
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Sources:
      IF (NSRCS.EQ.0) THEN
         WRITE (HILINE,1002) TSKNAM
         CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
      ELSE
C                                       Included or excluded:
         IF (DOSWNT) THEN
            WRITE (HILINE,1003) TSKNAM
         ELSE
            WRITE (HILINE,1004) TSKNAM
            END IF
         CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Write out sources (this method
C                                       avoids potential problems if
C                                       NSRCS is odd):
         KEYWRD = 'SOURCES  ='
         I = 1
 10      IF (NSRCS.GT.1) THEN
            WRITE (HILINE,1010)  TSKNAM, KEYWRD, CSRCS(I), CSRCS(I+1)
            CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            KEYWRD = '         '
            I = I + 2
            NSRCS = NSRCS - 2
            GO TO 10
            END IF
         IF (NSRCS.EQ.1) THEN
            WRITE (HILINE,1011) TSKNAM, KEYWRD, CSRCS(I)
            CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
         END IF
C                                       Stokes:
      CALL H2CHR (2, 1, XSTOKE, STOKES)
      WRITE (HILINE,1012) TSKNAM, STOKES
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Frequency ID:
      WRITE (HILINE,1013) TSKNAM, FREQID
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       IF range:
      WRITE (HILINE,1014) TSKNAM, BIF, EIF
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Time range:
      CALL HITIME (BTIME, ETIME, HISLUN, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Antennae:
      NANTS = 0
      DO 20 I = 1,MAXANT
         IF (CALANT(I)) THEN
            NANTS = NANTS + 1
            ANTS(NANTS) = I
            END IF
 20      CONTINUE
      WRITE (HILINE,1020) TSKNAM
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Write out antennae:
      IF (NANTS.EQ.MAXANT) THEN
         WRITE (HILINE,1021) TSKNAM
         CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      ELSE
         LIMIT = MIN (12, NANTS)
         WRITE (HILINE,1022) TSKNAM, (ANTS(I), I = 1, LIMIT)
         CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (LIMIT.LT.NANTS) THEN
            DO 30 I = 13, NANTS, 12
               LIMIT = I
               LIMIT2 = MIN (I + 11, NANTS)
               WRITE (HILINE,1023) TSKNAM, (ANTS(J), J = LIMIT, LIMIT2)
               CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 100
 30            CONTINUE
            END IF
         END IF
C                                       Subarray and CL version
      WRITE (HILINE,1030) TSKNAM, SUBARR, CLVER
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       TY table version
      WRITE (HILINE,1031) TSKNAM, TYVER
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C
      GO TO 200
C                                       HI file write errors:
 100  WRITE (MSGTXT,1100) IERR
      CALL MSGWRT (8)
C                                       Close HIstory file:
 200  CALL HICLOS (HISLUN, .TRUE., BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) IERR
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOLHIS: ERROR',I3,' OPENING HISTORY FILE')
 1001 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 1002 FORMAT (A6,' SOURCES =  ''''       / All sources selected')
 1003 FORMAT (A6,' / Sources included:')
 1004 FORMAT (A6,' / Sources excluded:')
 1010 FORMAT (A6,' ',A10,' ''',A16,''',''',A16,'''')
 1011 FORMAT (A6,' ',A10,' ''',A16,'''')
 1012 FORMAT (A6,' STOKES  = ''',A2,'''  / Stokes corrected')
 1013 FORMAT (A6,' FREQID  =',I3,'      / Frequency ID code')
 1014 FORMAT (A6,' BIF     =',I3,' EIF    =',I3)
 1020 FORMAT (A6,' / Reference antennae:')
 1021 FORMAT (A6,' ANTENNAS =   0         / All antennae selected')
 1022 FORMAT (A6,' ANTENNAS = ',12(I3,' '))
 1023 FORMAT (A6,'            ',12(I3,' '))
 1030 FORMAT (A6,' SUBARRAY = ',I3,' GAINVER = ',I3)
 1031 FORMAT (A6,' TYVER = ',I3,' / TY version no.')
 1100 FORMAT ('SOLHIS: ERROR',I3,' WRITING TO HISTORY FILE')
 1200 FORMAT ('SOLHIS: ERROR',I3,' CLOSING HISTORY FILE')
      END
      LOGICAL FUNCTION WNTSRC (SRC)
C-----------------------------------------------------------------------
C   Determine whether a given source is selected.
C
C   Input:
C     SRC         I         Source number
C
C   Return value:
C     WNTSRC      L         .TRUE. if source selected, .FALSE.
C                           otherwise
C
C   Inputs in common:
C     DOSWNT      L         .TRUE. if sources in SRCS are selected
C     NSRCS       I         Number of sources in SRCS
C     SRCS        I(*)      List of selected/deselected sources
C
C   If NSRCS = 0 then all sources are selected
C-----------------------------------------------------------------------
      INTEGER   SRC

      INTEGER   I
      LOGICAL   INSRCS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SOLCL.INC'
C-----------------------------------------------------------------------
      IF (NSRCS.EQ.0) THEN
         WNTSRC = .TRUE.
      ELSE
C                                       Check source against source
C                                       list:
         INSRCS = .FALSE.
         I = 0
 10      IF ((.NOT.INSRCS) .AND. (I.NE.NSRCS)) THEN
            I = I + 1
            IF (SRCS(I).EQ.SRC) INSRCS = .TRUE.
            GO TO 10
            END IF
         IF (DOSWNT) THEN
            WNTSRC = INSRCS
         ELSE
            WNTSRC = .NOT.INSRCS
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
      END
