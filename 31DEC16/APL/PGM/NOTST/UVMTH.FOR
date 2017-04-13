LOCAL INCLUDE 'UVMTH.INC'
C                                       Local include for UVMTH
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAME2(3),
     *   XCLAS2(2), XNAMOU(3), XCLAOU(2), XOPCOD
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH(3), XDOAC, XS2, XDISK2, XSOUT, XDISO,
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), BUFF3(UVBFSS)
      INTEGER   SEQIN, SEQ2, SEQOUT, DISKIN, DISK2, DISKO, ICODE,
     *   NUMHIS, JBUFSZ, BLPNT(MAXANT,MAXANT), CNOIN, CNOIN2, CNOOUT,
     *   CATO(256), NVSA, NRPRMA, LRECA, NVISA, ILOCW2, MAXXBL, NVIS2,
     *   LREC2, NRPRM2, CATOLD(256)
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAS2*6, NAMOUT*12,
     *   CLAOUT*6, OPCODE*4, HISCRD(10)*64
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XDOAC, XNAME2,
     *   XCLAS2, XS2, XDISK2, XNAMOU, XCLAOU, XSOUT, XDISO, XOPCOD
      COMMON /INTPRM/ CATO, CATOLD, SEQIN, SEQ2, SEQOUT, DISKIN, DISK2,
     *   DISKO, ICODE, NUMHIS, CNOIN, CNOIN2, CNOOUT, NVSA, NRPRMA,
     *   LRECA, NVISA, ILOCW2, MAXXBL, NVIS2, LREC2, NRPRM2
      COMMON /CHARPM/ NAMEIN, CLAIN, NAME2, CLAS2, NAMOUT, CLAOUT,
     *   OPCODE, HISCRD
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /AVGCOM/ BLPNT, BUFF3
LOCAL END
      PROGRAM UVMTH
C-----------------------------------------------------------------------
C! Averages one data set and applied it to another.
C# Utility UV UV-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2000, 2005, 2007-2008, 2010-2012,
C;  Copyright (C) 2015-2016
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
C   UVMTH will time average one data file and use the average to operate
C   on another file.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      IN2NAME        NAME2         Name of avg. file.
C      IN2CLASS       CLAS2         Class of avg. file.
C      IN2SEQ         SEQ2          Seq. no. of avg. file.
C      IN2DISK        DISK2         Vol. no. of avg. file.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      OPCODE         OPCODE        Operation code.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, NWORDS
      LONGINT   AVDOFF
      REAL      AVDATA(2)
      INCLUDE 'UVMTH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVMTH '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVMTIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Create averaging memory
      NWORDS = (3 * NVSA * MAXXBL - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, AVDATA, AVDOFF, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Average second input file.
      CALL UVMAVG (NVSA, AVDATA(1+AVDOFF), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Apply to first input file.
      CALL UVMTUV (NVSA, AVDATA(1+AVDOFF), IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVMHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVMTIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVMTIN gets input parameters for UVMTH and creates an output file.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in UVMTH for more details.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCFA, INCIFA, NUMAN(513), NANT, I,
     *   LUN
      REAL      RPARM(20)
      HOLLERITH CATH(256)
      LOGICAL   MATCH
      INCLUDE 'UVMTH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATH, CATBLK)
      DATA BLANK  /' '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 168
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
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      SEQ2 = IROUND (XS2)
      DISKIN = IROUND (XDISIN)
      DISK2 = IROUND (XDISK2)
      DISKO = IROUND (XDISO)
      DOACOR = XDOAC.GT.0.0
C                                       Interprete opcode default=SUB
      ICODE = 2
      IF (OPCODE.EQ.'ADD ') ICODE = 1
      IF (OPCODE.EQ.'SUB ') ICODE = 2
      IF (OPCODE.EQ.'MULT') ICODE = 3
      IF (OPCODE.EQ.'DIV ') ICODE = 4
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
C                                       Set time range.
      CALL RCOPY (8, XTIME, TIMRNG)
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = .FALSE.
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
C                                       Find first input file
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, PTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'READ', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       Get number of antennas from AN
C                                       file.
      LUN = 29
      CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN, BUFF1, NUMAN, IERR)
      IF (IERR.EQ.0) THEN
         IF (SUBARR.GT.NUMAN(1)) SUBARR = 1
         NANT = NUMAN(1+SUBARR)
         END IF
C                                       If failed, assume 28.
      IF ((IERR.NE.0) .OR. (NANT.EQ.0)) THEN
         MSGTXT = 'COULD NOT GET NUMBER OF ANTENNAS FROM AN FILES ' //
     *      'ASSUME 29'
         CALL MSGWRT (6)
         NANT = 29
         END IF
      MAXXBL = NANT * (NANT+1) / 2
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF1, IERR)
C                                       Save length, etc
      NVSA = (LREC - NRPARM) / CATBLK(KINAX)
      INCFA = INCF / CATBLK(KINAX)
      INCIFA = INCIF / CATBLK(KINAX)
      NRPRMA = NRPARM
      LRECA = LREC
      NVISA = NVIS
C                                       Create new file.
C                                       Get CATBLK from old file.
      CNOIN2 = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISK2, CNOIN2, NAME2, CLAS2, SEQ2,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAME2, CLAS2, SEQ2, DISK2, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK2, CNOIN2, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      CALL COPY (256, CATBLK, CATOLD)
C                                       Allow packed uv data
      ILOCW2 = -1
      IF (CATBLK(KINAX).EQ.1) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCW2,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Check input file compatibility
      IF ((NVSA.NE.(LREC-NRPARM)/CATBLK(KINAX)) .OR.
     *   (INCFA.NE.INCF/CATBLK(KINAX)) .OR.
     *   (INCIFA.NE.INCIF/CATBLK(KINAX))) THEN
         JERR = 4
         MSGTXT = 'INPUT FILES ARE INCOMPATIBLE'
         GO TO 990
         END IF
      NVIS2 = NVIS
      LREC2 = LREC
      NRPRM2 = NRPARM
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAME2, CLAS2, SEQ2, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CNOOUT = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CNOOUT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((CNOOUT.NE.CNOIN2) .OR. (DISKO.NE.DISKIN)) THEN
            MSGTXT = 'MAY OVERWRITE INPUT FILE ONLY.  QUITTING'
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, CNOOUT, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOOUT
      FRW(NCFILE) = FRW(NCFILE) - 1
      CALL COPY (256, CATBLK, CATO)
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISK2, CNOIN2, NAME2, CLAS2, SEQ2, PTYPE,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK2
      FCNO(NCFILE) = CNOIN2
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy header keywords
      CALL KEYCOP (DISKIN, CNOIN, DISKO, CNOOUT, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVMTIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1065 FORMAT ('UVMTIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE UVMAVG (NC, AVDATA, IRET)
C-----------------------------------------------------------------------
C   UVMTUV averages the second input file and saves it in an array.
C   Input:
C      NC       I        Number correlators in data set
C   Input in common:
C      DISKIN   I        Volumn number of the avg. file.
C      CNOIN    I        Catalog slot number for second input
C   Output in common:
C      BLPNT    I(*,*)   Baseline numbers for first and second antennas
C   Output:
C      AVDATA   R(*)     Time averaged baseline values (*,corr,baseline)
C      IRET     I        Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NC, IRET
      REAL      AVDATA(3,NC,*)
C
      INTEGER   INDEX, I, J, IA1, IA2, NUMBAS, BLINDX, NUMVS, NCP,
     *   CATMP(256)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVMTH.INC'
      REAL      RPARM(20), VIS(UVBFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (VIS, BUFF1)
C-----------------------------------------------------------------------
C                                       Initialize arrays
      J = 3 * NC * MAXXBL
      CALL RFILL (J, 0.0, AVDATA)
      NUMBAS = 0
      J = MAXANT * MAXANT
      CALL FILL (J, 0, BLPNT)
      NCP = 3 * NVSA
C                                       protect output image header
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      NUMVS = 0
C                                       Loop
C                                       Read vis. record.
 100  CONTINUE
         CALL UVGET ('READ', RPARM, VIS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IF (IRET.EQ.0) THEN
C                                       Baseline pointers
            IF (ILOCB.GE.0) THEN
               IA1 = RPARM(1+ILOCB) / 256. + 0.1
               IA2 = RPARM(1+ILOCB) - IA1*256. + 0.1
            ELSE
               IA1 = RPARM(1+ILOCA1) + 0.1
               IA2 = RPARM(1+ILOCA2) + 0.1
               END IF
            IF (BLPNT(IA1,IA2).LE.0) THEN
               IF (NUMBAS.GE.MAXXBL) THEN
                  WRITE (MSGTXT,1101) NUMBAS
                  IRET = 5
                  GO TO 990
                  END IF
               NUMBAS = NUMBAS + 1
               BLPNT(IA1,IA2) = NUMBAS
               BLPNT(IA2,IA1) = NUMBAS
               END IF
            BLINDX = BLPNT(IA1,IA2)
            INDEX = 1
            NUMVS = NUMVS + 1
C                                       Accumulate
            DO 150 J = 1,NVSA
               IF (VIS(INDEX+2).GT.0.0) THEN
                  AVDATA(1,J,BLINDX) = AVDATA(1,J,BLINDX) +
     *               VIS(INDEX) * VIS(INDEX+2)
                  AVDATA(2,J,BLINDX) = AVDATA(2,J,BLINDX) +
     *               VIS(INDEX+1) * VIS(INDEX+2)
                  AVDATA(3,J,BLINDX) = AVDATA(3,J,BLINDX) +
     *               VIS(INDEX+2)
                  END IF
               INDEX = INDEX + 3
 150           CONTINUE
            GO TO 100
            END IF
C                                       Average
 200  DO 220 I = 1,NUMBAS
         DO 210 J = 1,NVSA
            IF (AVDATA(3,J,I).GT.0.0) THEN
               AVDATA(1,J,I) = AVDATA(1,J,I) / AVDATA(3,J,I)
               AVDATA(2,J,I) = AVDATA(2,J,I) / AVDATA(3,J,I)
               END IF
 210        CONTINUE
 220     CONTINUE
      CALL COPY (256, CATMP, CATBLK)
C                                       MESSAGE
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),1210) NUMVS
      WRITE (MSGTXT,1210) NUMVS
      CALL MSGWRT (4)
C                                       Close file
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      IF (IRET.LE.0) GO TO 999
         WRITE (MSGTXT,1220) IRET
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVMAVG: ERROR',I3,' OPEN VIS FILE FOR AVERAGING')
 1100 FORMAT ('UVMAVG: ERROR',I3,' READING VIS FILE FOR AVERAGING')
 1101 FORMAT ('UVMAVG: TOO MANY BASELINES >',I5)
 1210 FORMAT ('Averaged ',I8,' visibility records')
 1220 FORMAT ('UVMAVG: ERROR',I3,' CLOSING AVG. VIS FILE')
      END
      SUBROUTINE UVMTUV (NC, AVDATA, IRET)
C-----------------------------------------------------------------------
C   UVMTUV sends uv data one point at a time to the correction
C   routine and then writes the modified data if requested.
C   Input in common:
C   Input:
C      NC       I      Number of correlators
C      AVDATA   R(*)   Average of data set 2 (3,NC,baseline)
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NC, IRET
      REAL      AVDATA(3,NC,*)
C
      CHARACTER OFILE*48, IFILE*48, OPCH(4)*12
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, ILENBU,
     *   KBIND, NIOUT, NIOLIM, IBIND, I, IA1, IA2, ISUB, BO, VO, NUMVIS,
     *   XCOUNT, LFQ, ITRIM, NCH, RNXRET
      LOGICAL   T, F
      INCLUDE 'UVMTH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
      DATA OPCH /'sum', 'difference', 'product', 'ratio'/
C-----------------------------------------------------------------------
C                                       Get uv header info.
      CALL UVPGET (IRET)
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISK2, CNOIN2, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISK2, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CNOOUT, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS2, VO, LREC2, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS2, VO, LREC2, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
      LFQ = 1
C                                       make an index table
      CALL RNXGET (DISK2, CNOIN2, CATOLD)
      CALL RNXINI (DISKO, CNOOUT, CATBLK, RNXRET)
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       Loop over buffer
         DO 190 I = 1,INIO
            IF (ILOCB.GE.0) THEN
               IA1 = BUFF1(IPTRI+ILOCB) / 256. + 0.1
               IA2 = BUFF1(IPTRI+ILOCB) - IA1*256. + 0.1
               ISUB = (BUFF1(IPTRI+ILOCB) - IA1*256 - IA2) * 100.0 + 1.1
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               ISUB = BUFF1(IPTRI+ILOCSA) + 0.1
               END IF
            IF (ILOCFQ.GE.0) LFQ = BUFF1(IPTRI+ILOCFQ) + 0.01
            IF ((ISUB.EQ.SUBARR) .AND. (LFQ.EQ.FRQSEL)) THEN
               NUMVIS = NUMVIS + 1
C                                      Call user routine.
               IF (ILOCW2.GE.0) THEN
                  CALL ZUVXPN (NVSA, BUFF1(IPTRI+NRPRM2),
     *               BUFF1(IPTRI+ILOCW2), BUFF3)
                  CALL UVMATH (IA1, IA2, NC, AVDATA, BUFF3, IRET)
                  CALL ZUVPAK (NVSA, BUFF3, BUFF1(IPTRI+ILOCW2),
     *               BUFF1(IPTRI+NRPRM2))
               ELSE
                  CALL UVMATH (IA1, IA2, NC, AVDATA,
     *               BUFF1(IPTRI+NRPRM2), IRET)
                  END IF
C                                       Error (fatal)
               IF (IRET.GT.0) THEN
 120              WRITE (MSGTXT,1120) IRET
                  GO TO 990
C                                       Copy to output.
               ELSE IF (IRET.EQ.0) THEN
                  XCOUNT = XCOUNT + 1.0D0
                  CALL RCOPY (LREC2, BUFF1(IPTRI), BUFF2(IPTRO))
                  IPTRO = IPTRO + LREC2
                  NIOUT = NIOUT + 1
C                                       update NX table
                  CALL RNXUPD (BUFF1(IPTRI), RNXRET)
                  END IF
               IPTRI = IPTRI + LREC2
C                                       Write vis record.
               IF (NIOUT.GE.NIOLIM) THEN
                  CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1150) IRET
                     GO TO 990
                     END IF
                  IPTRO = KBIND
                  NIOUT = 0
                  END IF
               END IF
 190        CONTINUE
C                                       Read next buffer.
         IF (INIO.GT.0) GO TO 100
C                                       Finish write
 200  NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      NCH = ITRIM (OPCH(ICODE))
      WRITE (MSGTXT,1200) NVIS, OPCH(ICODE)(:NCH)
      CALL MSGWRT (4)
      CALL UCMPRS (NVIS, DISKO, CNOOUT, LUNO, CATBLK, IRET)
C                                       close NX table
      CALL RNXCLS (RNXRET)
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVMTUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('UVMTUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('UVMTUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('UVMTUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('UVMTUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('UVMTUV: UVMATH ERROR',I3)
 1150 FORMAT ('UVMTUV: ERROR',I3,' WRITING VIS FILE')
 1200 FORMAT ('Wrote',I12,1X,A,' visibility records')
      END
      SUBROUTINE UVMHIS
C-----------------------------------------------------------------------
C   UVMHIS copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, LABEL*8, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      INCLUDE 'UVMTH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA NONOT, NOTTYP /1, 'NX'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISK2, DISKO, CNOIN2, CNOOUT, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      HILINE = TSKNAM // '/ data set averaged over all time'
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       calibration history
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       data set acted upon
      HILINE = TSKNAM // '/ data set acted upon by the averaged data'
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       OPCODE
      WRITE (HILINE,2000) TSKNAM, OPCODE
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                      Add other supplied history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1010) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 100
 50         CONTINUE
         END IF
C                                       Close HI file
 100  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISK2, DISKO, CNOIN2,
     *   CNOOUT, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'UVMHIS: ERROR COPYING TABLES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, CNOOUT, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVMHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/')
 2000 FORMAT (A6,'OPCODE =''',A,''' / Operation requested')
      END
      SUBROUTINE UVMATH (IA1, IA2, NC, AVDATA, VIS, IRET)
C-----------------------------------------------------------------------
C   Modify data.
C   Inputs:
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      BLPNT  I(*,*)    Baseline numbers for first and second antennas
C      AVDATA R(3,*,*)  Time averaged baseline values (*,corr,baseline)
C      NVSA   I         Number of correlations
C   Output:
C      VIS        R    Visibilities
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   IA1, IA2, NC, IRET
      REAL      AVDATA(3,NC,*), VIS(3,*)
C
      INTEGER   BLINDX, LOOP
      COMPLEX   C1, C2, C3
      INCLUDE 'UVMTH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Something for this baseline?
      BLINDX = BLPNT(IA1,IA2)
      IF (BLINDX.LE.0) THEN
         IRET = -1
         GO TO 999
         END IF
C                                       Branch by operation
C                                       Add
      IF (ICODE.LE.1) THEN
         DO 10 LOOP = 1,NVSA
            IF (AVDATA(3,LOOP,BLINDX).GT.0.0) THEN
               VIS(1,LOOP) = VIS(1,LOOP) + AVDATA(1,LOOP,BLINDX)
               VIS(2,LOOP) = VIS(2,LOOP) + AVDATA(2,LOOP,BLINDX)
            ELSE
               VIS(3,LOOP) = 0.0
               END IF
 10         CONTINUE
C                                       Subtract
      ELSE IF (ICODE.EQ.2) THEN
         DO 20 LOOP = 1,NVSA
            IF (AVDATA(3,LOOP,BLINDX).GT.0.0) THEN
               VIS(1,LOOP) = VIS(1,LOOP) - AVDATA(1,LOOP,BLINDX)
               VIS(2,LOOP) = VIS(2,LOOP) - AVDATA(2,LOOP,BLINDX)
            ELSE
               VIS(3,LOOP) = 0.0
               END IF
 20         CONTINUE
C                                       Multiply
      ELSE IF (ICODE.EQ.3) THEN
         DO 30 LOOP = 1,NVSA
            IF (AVDATA(3,LOOP,BLINDX).GT.0.0) THEN
               C1 = CMPLX (VIS(1,LOOP), VIS(2,LOOP))
               C2 = CMPLX (AVDATA(1,LOOP,BLINDX), AVDATA(2,LOOP,BLINDX))
               C3 = C1 * C2
               VIS(1,LOOP) = REAL (C3)
               VIS(2,LOOP) = AIMAG (C3)
            ELSE
               VIS(3,LOOP) = 0.0
               END IF
 30         CONTINUE
C                                       Divide
      ELSE
         DO 40 LOOP = 1,NVSA
            IF (AVDATA(3,LOOP,BLINDX).GT.0.0) THEN
               C1 = CMPLX (VIS(1,LOOP), VIS(2,LOOP))
               C2 = CMPLX (AVDATA(1,LOOP,BLINDX), AVDATA(2,LOOP,BLINDX))
               IF (ABS (C2).GT.1.0E-10) THEN
                  C3 = C1 / C2
               ELSE
                  C3 = CMPLX (0.0, 0.0)
                  VIS(3,LOOP) = 0.0
                  END IF
               VIS(1,LOOP) = REAL (C3)
               VIS(2,LOOP) = AIMAG (C3)
            ELSE
               VIS(3,LOOP) = 0.0
               END IF
 40         CONTINUE
         END IF
C
 999  RETURN
      END
