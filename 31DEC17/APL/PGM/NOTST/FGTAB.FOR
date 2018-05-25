LOCAL INCLUDE 'FGTAB.INC'
C                                       Local include for FGTAB
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
C                                       Input parameters
      HOLLERITH XNAMEI(3), XCLAIN(2), XOUTFI(12), XOPCOD
      REAL      XSIN, XDISIN, XNVER, BUFF1(UVBFSS)
      INTEGER   SEQIN, DISKIN, CNOIN, IVER
      CHARACTER NAMEIN*12, CLAIN*6, OUTFIL*48, OPCODE*4
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNVER, XOUTFI,
     *   XOPCOD
      COMMON /VGNCOM/ BUFF1, SEQIN, DISKIN, CNOIN, IVER
      COMMON /VGNCHR/ NAMEIN, CLAIN, OUTFIL, OPCODE
C                                                          End FGTAB
LOCAL END
      PROGRAM FGTAB
C-----------------------------------------------------------------------
C! Prints freq range data from an FG table
C# UV Plot EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2016
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
C   FGTAB prints FG extension file with freq range
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      INVERS.....Version number of table to plot, 0=>1.
C      DOCRT......> 0 on CRT
C      OUTPRINT...text file
C-----------------------------------------------------------------------
C
      CHARACTER PRGN*6
      INTEGER   IRET
      INCLUDE 'FGTAB.INC'
      DATA PRGN /'FGTAB '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL FGPIN (PRGN, IRET)
C                                       Close down
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FGPIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   Gets the inputs parameters for FGTAB and prepare data for plot
C   reading FG table
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FGTAB.INC'
C
      INTEGER   BUFF(256), I, BUFFER(512), IROUND, LUNI, VER, IFGRNO,
     *   FGKOLS(MAXFGC), FGNUMV(MAXFGC), NFGROW, SOURID, SUBA, FREQID,
     *   ANTFG(2), IFS(2), CHANS(2), LUNP, FINDP, NPARMS, NI, NUMIF,
     *   ANX, ANN
      REAL      TIMER(2)
      CHARACTER STAT*4, PRGN*6, TYPTMP*2, REASON*24
      LOGICAL   F, PFLAGS(4)
      DOUBLE PRECISION F1, F2, FX, FN
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DCHND.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      NPARMS = 21
C                                        Get input parameters.
      CALL SETUP (PRGN, NPARMS, XNAMEI, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GETTING INPUT PARAMETERS'
         IRET = 8
         RQUICK = F
         GO TO 990
         END IF
C                                       Decode inputs.
C                                       characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (48, 1, XOUTFI, OUTFIL)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
C                                       Integers
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      IVER = IROUND (XNVER)

      CNOIN = 1
      TYPTMP = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, TYPTMP,
     *   NLUSER, STAT, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      'UV', NLUSER
         GO TO 990
         END IF
C                                       Save name class etc.
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      XDISIN = DISKIN
      XSIN = SEQIN
C                                       Read catalog header
      STAT = 'READ'
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, STAT, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING UV DATA HEADER'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = CNOIN
      FVOL(NCFILE) = DISKIN
      FRW(NCFILE) = 0
      XDISIN = DISKIN
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 990
      SEQIN = CATBLK(KIIMS)
      XSIN = SEQIN
C                                       get FQ settings first
      VER = 1
      LUNI = 27
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, VER, CATBLK, LUNI,
     *   NUMIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING FQ TABLE FOR FREQUENCIES'
         GO TO 990
         END IF
C                                       read FG table first time to
C                                       store number of records for
C                                       each ant, BL, all flags
C                                       Open FG file
      LUNI = 27
      CALL FLGINI ('READ', BUFFER, DISKIN, CNOIN, IVER, CATBLK, LUNI,
     *  IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING FLAG TABLE'
         GO TO 990
         END IF
C                                       # rows in the table
      NFGROW = BUFFER(5)
C                                       Open output device
      LUNP = 3
      IF (OUTFIL.EQ.' ') OUTFIL = 'HOME:FGTAB.output'
      CALL ZTXOPN ('WRIT', LUNP, FINDP, OUTFIL, .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT FILE'
         GO TO 990
         END IF
C                                       Loop and copy
      DO 50 I = 1,NFGROW
         CALL TABFLG ('READ', BUFFER, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *      SUBA, FREQID, ANTFG, TIMER, IFS, CHANS, PFLAGS, REASON,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING FLAG TABLE'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
            IF (SOURID.GT.0) GO TO 50
            IF (SUBA.GT.0) GO TO 50
            IF (FREQID.GT.0) GO TO 50
            IF (TIMER(1).GT.0.0) GO TO 50
            IF (TIMER(2).LT.100.) GO TO 50
            ANX = MAX (ANTFG(1), ANTFG(2))
            ANN = MIN (ANTFG(1), ANTFG(2))
            IF (ANN.GT.0) GO TO 50
            IF ((OPCODE.NE.'ANTE') .AND. (ANX.GT.0)) GO TO 50
C                                       write freq range(s)
            CHANS(1) = MAX (1, CHANS(1))
            IF ((CHANS(2).LT.CHANS(1)) .OR.
     *         (CHANS(2).GT.CATBLK(KINAX+JLOCF)))
     *         CHANS(2) = CATBLK(KINAX+JLOCF)
            IFS(1) = MAX (1, IFS(1))
            IF (JLOCIF.LT.0) THEN
               IFS(2) = 1
            ELSE IF ((IFS(2).LT.IFS(1)) .OR.
     *         (IFS(2).GT.CATBLK(KINAX+JLOCIF))) THEN
               IFS(2) = CATBLK(KINAX+JLOCIF)
               END IF
            DO 40 NI = IFS(1),IFS(2)
               F1 = CATD(KDCRV+JLOCF) + FOFF(NI) + (CHANS(1) -
     *            CATR(KRCRP+JLOCF)) * FINC(NI)
               F2 = CATD(KDCRV+JLOCF) + FOFF(NI) + (CHANS(2) -
     *            CATR(KRCRP+JLOCF)) * FINC(NI)
               F1 = F1 / 1.D6
               F2 = F2 / 1.D6
               FN = MIN (F1, F2)
               FX = MAX (F1, F2)
               IF (ANX.LE.0) THEN
                  WRITE (MSGTXT,1050) FN, FX
               ELSE
                  WRITE (MSGTXT,1051) FN, FX, ANX
                  END IF
               CALL MSGWRT (4)
               CALL ZTXIO ('WRIT', LUNP, FINDP, MSGTXT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING TEXT FILE'
                  GO TO 990
                  END IF
 40            CONTINUE
            END IF
 50      CONTINUE
      CALL ZTXCLS (LUNP, FINDP, IRET)
C                                       Close the table
      CALL TABIO ('CLOS', 0, IFGRNO, BUFFER, BUFFER, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FGPIN ERROR:',I7,1X,A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' TYPE=',A2,' USER=',I4)
 1050 FORMAT  (' BFREQ =',F14.6,'  EFREQ=',F14.6,' /')
 1051 FORMAT  (' BFREQ =',F14.6,'  EFREQ=',F14.6,'  ANTENNAS =',I4,' /')
      END
