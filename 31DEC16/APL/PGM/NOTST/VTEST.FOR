LOCAL INCLUDE 'VTEST.INC'
      INCLUDE 'INCS:DSEL.INC'
C                                      Character declerations
      CHARACTER  NAMEIN*12, CLAIN*6
      COMMON /CHPARM/ NAMEIN, CLAIN
C                                      Input parameters
      HOLLERITH  XNAMEI(3), XCLAIN(2)
      REAL       XSIN, XDISIN, CPARM(10), DPARM(10), FLD
      INTEGER   SEQIN, CNOIN, DISKIN, D0, D1, D2, IY
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, CPARM, DPARM,
     *   SEQIN, CNOIN, DISKIN, FLD, D0, D1, D2, IY
C                                      Misc. parameters
      DOUBLE PRECISION RAPP0, DAPP0, FRFROM
      INTEGER   KSEQO, NUMFRQ, INCFU, INCIFU, INCSU
      LOGICAL   WUVCMP, NEWSCN, NEWOFF
      COMMON /OPARM/ RAPP0, DAPP0, FRFROM, KSEQO, NUMFRQ, INCFU, INCIFU,
     *   INCSU, WUVCMP, NEWSCN, NEWOFF
LOCAL END
      PROGRAM VTEST
C-----------------------------------------------------------------------
C! VTEST shifts spectral-line single-dish data to a specified velocity.
C# Calibration Spectral UV VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2007
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
C   VTEST will shift spectral line data to a specified velocity.  The
C   program assumes that the telescope has tracked the velocity in the
C   header for the reference coordinates in the header.  It then shifts
C   each spectrum so that that velocity is wrt the actual coordinate
C   observed rather than the reference coordinate.  It works only on
C   single-source, single-dish uv data sets.
C   Inputs:
C      AIPS adverb          Description.
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      DPARM......Controls:
C                 (1) Tells the task when to compute the reference
C                 doppler shift.
C                 0 -> only at the start using the reference coordinate.
C                 1 -> only at the start, but using the first coordinate.
C                 2 -> on each "off" scan (detected by a missing scan
C                 number) using the last coordinate before the off.
C                 3 -> on each scan using the last coordinate of the
C                 previous scan.
C                 (2) A warning is issued if the shift exceeds DPARM(2)
C                 channels.  0 -> NCHAN/20.  The task limits the number
C                 of consecutive samples warned to 10 but if there are
C                 then good samples, it will resume warnings on the next
C                 "bad".
C                 (3) A sample is deleted if the shift exceeds DPARM(3)
C                 channels.  0 -> NCHAN.  This may be used to detect and
C                 flag periods in which the antenna is blown way off
C                 course.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET, BUFFER(256)
      INCLUDE 'VTEST.INC'
      DATA PRGM /'VTEST '/
C-----------------------------------------------------------------------
C                                       Get input parameters
C                                       and open O/P file.
      CALL SDVLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Max over all Ras, Decs
      IF (DPARM(7).LE.0.0) THEN
         CALL CVRADE
C                                       Max over all UTs, Day #s
      ELSE
         CALL CVUTDA
         END IF
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE SDVLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   SDVLIN gets the input parms for VTEST, fins the input file
C   opens the output file and performs some checks on the
C   validity of the VTEST operation.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C     JERR    I     Error code: 0 => ok
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH CATH(256)
      CHARACTER STAT*4, PRGN*6, UTYPE*2
      INTEGER   JERR, NPARM, IROUND, IERR, BUFF1(512)
      INCLUDE 'VTEST.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (CATBLK, CATH)
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 27
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Check file exists
      IF (DPARM(6).LE.0.0) THEN
         CALL H2CHR (12, 1, XNAMEI, NAMEIN)
         CALL H2CHR (6, 1, XCLAIN, CLAIN)
         SEQIN = IROUND (XSIN)
         DISKIN = IROUND (XDISIN)
         CNOIN = 1
         UTYPE = 'UV'
         CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN,
     *      UTYPE, NLUSER, STAT, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1001) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *         NLUSER
            GO TO 990
            END IF
C                                       If it does extract the CATBLK
         CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1002) IERR
            GO TO 990
            END IF
C                                       Get uv header info.
         CALL UVPGET (JERR)
         IF (JERR.NE.0) GO TO 999
C                                       Fill AN information
C                                       into common in D/CANS.INC
         CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, BUFF1, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1070) JERR
            GO TO 990
            END IF
C                                       Disk, volume no.
         IUDISK = DISKIN
         IUCNO = CNOIN
C                                       Copy CATBLK to CATUV
         CALL COPY (256, CATBLK, CATUV)
         END IF
C                                       Default input parameters
      IF (CPARM(1).EQ.0.0) CPARM(1) = 0.0
      IF (CPARM(2).EQ.0.0) CPARM(2) = 350.0
      IF (CPARM(3).EQ.0.0) CPARM(3) = 10.0
      IF (CPARM(4).EQ.0.0) CPARM(4) = -30.0
      IF (CPARM(5).LT.CPARM(4)) CPARM(5) = 85.0
      IF (CPARM(6).EQ.0.0) CPARM(6) = 5.
      IF (CPARM(7).EQ.0.0) CPARM(7) = 0.0
      IF (CPARM(8).EQ.0.0) CPARM(8) = 23.0
      IF (CPARM(9).EQ.0.0) CPARM(9) = 6.0
      D0 = IROUND (DPARM(1))
      D1 = IROUND (DPARM(2))
      D2 = IROUND (DPARM(3))
      IY = IROUND (DPARM(4))
      FLD = DPARM(5) / 2.0
      IF (D0.LT.0) D0 = 0
      IF (D1.LT.D0) D1 = 350
      IF (D2.LE.0) D2 = 30
      IF (IY.LT.1950) IY = 1997
      IF (FLD.LE.0.1) FLD = 1.0
      JERR = 0
      GO TO 999
C                                       Write the error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDVLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1001 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1002 FORMAT ('SDVLIN: ERROR',I3,' COPYING CATBLK')
 1070 FORMAT ('SDVLIN: ERROR',I3,' OBTAINING ANTENNA INFORMATION')
      END
      SUBROUTINE CVRADE
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   IRDAY
      DOUBLE PRECISION XRA, XDEC, DVL, DXANT, DYANT, DZANT, XXR, XXD,
     *   UT, DOPVEL, VSUN, XXV, XDR, DOPVVV, DVH
      REAL      RA, DEC, DRA, UUT, RAML, DEML, RAMH, DEMH
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'VTEST.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Correct station positions for
C                                       centre array offset if non-zero
      IF (DPARM(6).LE.0.0) THEN
         DXANT = CNTRX + STNX(1)
         DYANT = -CNTRY - STNY(1)
         DZANT = CNTRZ + STNZ(1)
      ELSE
         DXANT = 0.0D0
         DYANT = 0.0D0
         DZANT = 0.0D0
         END IF
C                                       Report parms, display header
      WRITE (MSGTXT,1000) 'RA ',CPARM(1),CPARM(2),CPARM(3)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1000) 'DEC',CPARM(4),CPARM(5),CPARM(6)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1001) FLD
      CALL MSGWRT (5)
      WRITE (MSGTXT,1002)
      CALL MSGWRT (5)
C                                       loop over days, UTs
      DO 220 IRDAY = D0,D1,D2
         UUT = CPARM(7) - CPARM(9)
 10      UUT = UUT + CPARM(9)
         IF (((UUT.LE.CPARM(8)) .AND. (CPARM(9).GT.0.0)) .OR.
     *      ((UUT.GE.CPARM(8)) .AND. (CPARM(9).LT.0.0))) THEN
            UT = UUT / 24.0
C                                       find max over Ras, Decs
            DVL = 0.0
            DVH = 0.0
            DEC = CPARM(4) - CPARM(6)
 20         DEC = DEC + CPARM(6)
            IF (((DEC.LE.CPARM(5)) .AND. (CPARM(6).GT.0.0)) .OR.
     *         ((DEC.GE.CPARM(5)) .AND. (CPARM(6).LT.0.0))) THEN
               XDEC = DEC * DG2RAD
               DRA = CPARM(3) / COS (XDEC)
               XDR = FLD * DG2RAD / COS (XDEC)
               RA = CPARM(1) - DRA
 30            RA = RA + DRA
               IF (((RA.LE.CPARM(2)) .AND. (DRA.GT.0.0)) .OR.
     *            ((RA.GE.CPARM(2)) .AND. (DRA.LT.0.0))) THEN
                  XRA = RA * DG2RAD
                  CALL DOPLR (XRA, XDEC, IY, IRDAY, UT, DXANT, DYANT,
     *               DZANT, VSUN, DOPVEL)
                  DOPVVV = DOPVEL - VSUN
                  XXD = XDEC + FLD * DG2RAD
                  XXR = XRA + XDR
                  CALL DOPLR (XXR, XXD, IY, IRDAY, UT, DXANT, DYANT,
     *               DZANT, VSUN, XXV)
                  IF (ABS(XXV-DOPVEL).GT.DVL) THEN
                     RAML = RA
                     DEML = DEC
                     DVL = ABS(XXV-DOPVEL)
                     END IF
                  IF (ABS(XXV-VSUN-DOPVVV).GT.DVH) THEN
                     RAMH = RA
                     DEMH = DEC
                     DVH = ABS(XXV-VSUN-DOPVVV)
                     END IF
                  XXR = XRA - XDR
                  CALL DOPLR (XXR, XXD, IY, IRDAY, UT, DXANT, DYANT,
     *               DZANT, VSUN, XXV)
                  IF (ABS(XXV-DOPVEL).GT.DVL) THEN
                     RAML = RA
                     DEML = DEC
                     DVL = ABS(XXV-DOPVEL)
                     END IF
                  IF (ABS(XXV-VSUN-DOPVVV).GT.DVH) THEN
                     RAMH = RA
                     DEMH = DEC
                     DVH = ABS(XXV-VSUN-DOPVVV)
                     END IF
                  XXD = XDEC - FLD * DG2RAD
                  CALL DOPLR (XXR, XXD, IY, IRDAY, UT, DXANT, DYANT,
     *               DZANT, VSUN, XXV)
                  IF (ABS(XXV-DOPVEL).GT.DVL) THEN
                     RAML = RA
                     DEML = DEC
                     DVL = ABS(XXV-DOPVEL)
                     END IF
                  IF (ABS(XXV-VSUN-DOPVVV).GT.DVH) THEN
                     RAMH = RA
                     DEMH = DEC
                     DVH = ABS(XXV-VSUN-DOPVVV)
                     END IF
                  XXR = XRA + XDR
                  CALL DOPLR (XXR, XXD, IY, IRDAY, UT, DXANT, DYANT,
     *               DZANT, VSUN, XXV)
                  IF (ABS(XXV-DOPVEL).GT.DVL) THEN
                     RAML = RA
                     DEML = DEC
                     DVL = ABS(XXV-DOPVEL)
                     END IF
                  IF (ABS(XXV-VSUN-DOPVVV).GT.DVH) THEN
                     RAMH = RA
                     DEMH = DEC
                     DVH = ABS(XXV-VSUN-DOPVVV)
                     END IF
                  GO TO 30
                  END IF
               GO TO 20
               END IF
C
            WRITE (MSGTXT,1200) IY, IRDAY, UUT, DVL, DVH, RAML, DEML
            CALL MSGWRT (5)
            GO TO 10
            END IF
 220     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('max over ',A,' = ',F7.2,' to ',F7.2,' by ',F7.3)
 1001 FORMAT ('RA and Dec both offset by plus/minus',F5.1,' degrees')
 1002 FORMAT ('  year',3X,'day',3X,'hour   Max LSR   helio',
     *   ' m/s',2X,'RA/Dec of max LSR')
 1200 FORMAT (I6,I6,F7.1,2F9.1,4X,2F8.1)
      END
      SUBROUTINE CVUTDA
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   IRDAY, IDML, IDMH
      DOUBLE PRECISION XRA, XDEC, DVL, DXANT, DYANT, DZANT, XXR, XXD,
     *   UT, DOPVEL, VSUN, XXV, XDR, DOPVVV, DVH
      REAL      RA, DEC, DRA, UUT, UTML, UTMH
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'VTEST.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Correct station positions for
C                                       centre array offset if non-zero
      IF (DPARM(6).LE.0.0) THEN
         DXANT = CNTRX + STNX(1)
         DYANT = -CNTRY - STNY(1)
         DZANT = CNTRZ + STNZ(1)
      ELSE
         DXANT = 0.0D0
         DYANT = 0.0D0
         DZANT = 0.0D0
         END IF
C                                       Report parms, display header
      WRITE (MSGTXT,1000) 'UT  ', CPARM(7), CPARM(8), CPARM(9)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1001) 'Day#', D0, D1, D2
      CALL MSGWRT (5)
      WRITE (MSGTXT,1002) FLD
      CALL MSGWRT (5)
      WRITE (MSGTXT,1003)
      CALL MSGWRT (5)
C                                       loop over RA and Dec
      DEC = CPARM(4) - CPARM(6)
 10   DEC = DEC + CPARM(6)
      IF (((DEC.LE.CPARM(5)) .AND. (CPARM(6).GT.0.0)) .OR.
     *   ((DEC.GE.CPARM(5)) .AND. (CPARM(6).LT.0.0))) THEN
         XDEC = DEC * DG2RAD
         DRA = CPARM(3) / COS (XDEC)
         XDR = FLD * DG2RAD / COS (XDEC)
         RA = CPARM(1) - DRA
 20      RA = RA + DRA
         IF (((RA.LE.CPARM(2)) .AND. (DRA.GT.0.0)) .OR.
     *      ((RA.GE.CPARM(2)) .AND. (DRA.LT.0.0))) THEN
            XRA = RA * DG2RAD
C                                       find max over Ras, Decs
            DVL = 0.0
            DVH = 0.0
            DO 200 IRDAY = D0,D1,D2
               UUT = CPARM(7) - CPARM(9)
 30            UUT = UUT + CPARM(9)
               IF (((UUT.LE.CPARM(8)) .AND. (CPARM(9).GT.0.0)) .OR.
     *            ((UUT.GE.CPARM(8)) .AND. (CPARM(9).LT.0.0))) THEN
                  UT = UUT / 24.0
                  CALL DOPLR (XRA, XDEC, IY, IRDAY, UT, DXANT, DYANT,
     *               DZANT, VSUN, DOPVEL)
                  DOPVVV = DOPVEL - VSUN
                  XXD = XDEC + FLD * DG2RAD
                  XXR = XRA + XDR
                  CALL DOPLR (XXR, XXD, IY, IRDAY, UT, DXANT, DYANT,
     *               DZANT, VSUN, XXV)
                  IF (ABS(XXV-DOPVEL).GT.DVL) THEN
                     UTML = UUT
                     IDML = IRDAY
                     DVL = ABS(XXV-DOPVEL)
                     END IF
                  IF (ABS(XXV-VSUN-DOPVVV).GT.DVH) THEN
                     UTMH = UUT
                     IDMH = IRDAY
                     DVH = ABS(XXV-VSUN-DOPVVV)
                     END IF
                  XXR = XRA - XDR
                  CALL DOPLR (XXR, XXD, IY, IRDAY, UT, DXANT, DYANT,
     *               DZANT, VSUN, XXV)
                  IF (ABS(XXV-DOPVEL).GT.DVL) THEN
                     UTML = UUT
                     IDML = IRDAY
                     DVL = ABS(XXV-DOPVEL)
                     END IF
                  IF (ABS(XXV-VSUN-DOPVVV).GT.DVH) THEN
                     UTMH = UUT
                     IDMH = IRDAY
                     DVH = ABS(XXV-VSUN-DOPVVV)
                     END IF
                  XXD = XDEC - FLD * DG2RAD
                  CALL DOPLR (XXR, XXD, IY, IRDAY, UT, DXANT, DYANT,
     *               DZANT, VSUN, XXV)
                  IF (ABS(XXV-DOPVEL).GT.DVL) THEN
                     UTML = UUT
                     IDML = IRDAY
                     DVL = ABS(XXV-DOPVEL)
                     END IF
                  IF (ABS(XXV-VSUN-DOPVVV).GT.DVH) THEN
                     UTMH = UUT
                     IDMH = IRDAY
                     DVH = ABS(XXV-VSUN-DOPVVV)
                     END IF
                  XXR = XRA + XDR
                  CALL DOPLR (XXR, XXD, IY, IRDAY, UT, DXANT, DYANT,
     *               DZANT, VSUN, XXV)
                  IF (ABS(XXV-DOPVEL).GT.DVL) THEN
                     UTML = UUT
                     IDML = IRDAY
                     DVL = ABS(XXV-DOPVEL)
                     END IF
                  IF (ABS(XXV-VSUN-DOPVVV).GT.DVH) THEN
                     UTMH = UUT
                     IDMH = IRDAY
                     DVH = ABS(XXV-VSUN-DOPVVV)
                     END IF
                  GO TO 30
                  END IF
 200           CONTINUE
            WRITE (MSGTXT,1200) IY, RA, DEC, DVL, DVH, IDML, UTML
            CALL MSGWRT (5)
            GO TO 20
            END IF
         GO TO 10
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('max over ',A,' =',F6.2,' to',F6.2,' by',F6.2)
 1001 FORMAT ('max over ',A,' =',I6,' to',I6,' by',I6)
 1002 FORMAT ('RA and Dec both offset by plus/minus',F5.1,' degrees')
 1003 FORMAT ('  year',4X,'RA',6X,'Dec    Max LSR   helio',
     *   ' m/s',2X,'Day/UT of max LSR')
 1200 FORMAT (I6,2F8.2,2F9.1,I9,F6.2)
      END
