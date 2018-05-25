LOCAL INCLUDE 'RLCOR.INC'
C                                       Local include for RLCOR
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF,
     *   XSOUT, XDISO, XCENT, CLCORP(20), SCRBUF(256), BUFF1(UVBFSS),
     *   BUFF2(UVBFSS), PARM(2,20), DIFPIX
      INTEGER   CATOLD(256), SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECO,
     *   NRPRMI, NRPRMO, OLDCNO, NEWCNO, LBIF, LEIF, ISUBAR, IFQID,
     *   IBUFF1(UVBFSS), IBUFF2(UVBFSS)
      LOGICAL   ISCOMP
      DOUBLE PRECISION UVSCAL
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF2, BUFF2)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XNAMOU, XCLAOU, XSOUT, XDISO, XCENT, CLCORP
      COMMON /RLCORP/ CATOLD, UVSCAL, SEQIN, SEQOUT, DISKIN, DISKO,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECO,
     *   NRPRMI, NRPRMO, ISCOMP, OLDCNO, NEWCNO, LBIF, LEIF, PARM,
     *   ISUBAR, IFQID, DIFPIX
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ SCRBUF, BUFF1, BUFF2, JBUFSZ
C                                       End local include for RLCOR
LOCAL END
      PROGRAM RLCOR
C-----------------------------------------------------------------------
C! Correct RL and LR for Right-left phase difference
C# Utility UV  Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2009, 2011, 2014-2015
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
C   RLCOR corrects a data set for the right-left phase difference in
C   up to 20 IFs.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      CLCORPRM(20    CLCORP        Phase differences
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'RLCOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'RLCOR '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL RLCORI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL RLCORU (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL RLCORH
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE RLCORI (PRGN, JERR)
C-----------------------------------------------------------------------
C   RLCORI gets input parameters for RLCOR and creates an output file
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, LUN, NIF
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'RLCOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK  /' '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 41
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, SCRBUF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 1
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Channel selection?
      IF (JLOCIF.LT.0) THEN
         LBIF = 1
         LEIF = 1
      ELSE
         LBIF = IROUND (XBIF)
         LEIF = IROUND (XEIF)
         LBIF = MIN (MAX (1, LBIF), CATBLK(KINAX+JLOCIF))
         IF (LEIF.LT.LBIF) LEIF = CATBLK(KINAX+JLOCIF)
         END IF
      IF (LEIF-LBIF.GT.19) THEN
         MSGTXT = 'YOU MAY DO ONLY 20 IFS AT A TIME'
         JERR = 10
         GO TO 990
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, SCRBUF, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, SCRBUF, IERR)
      ISUBAR = MAX (1, SUBARR)
      IFQID = MAX (1, FRQSEL)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       Put new values in CATBLK.
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.LE.0.0) THEN
         UVSCAL = 1.0D0
         DIFPIX = 0.0
      ELSE
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
         UVSCAL = CATD(KDCRV+JLOCF) / FREQ
         END IF
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       read compressed => write compr.
      IF (ISCOMP) THEN
         CATBLK(KINAX) = 1
         I = CATBLK(KIPCN)
         CALL CHR2H (8, 'WEIGHT  ', 1, CATBLK(KHPTP+2*I))
         CALL CHR2H (8, 'SCALE   ', 1, CATBLK(KHPTP+2*I+2))
         CATBLK(KIPCN) = I + 2
         ILOCWT = I
         END IF
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            WRITE (MSGTXT,1060)
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', SCRBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
      NEWCNO = CCNO
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Copy any header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
C                                       interpret parms
      NIF = LEIF - LBIF + 1
      DO 20 I = 1,NIF
         PARM(1,I) = COS (CLCORP(I) * DG2RAD)
         PARM(2,I) = SIN (CLCORP(I) * DG2RAD)
 20      CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RLCORI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('RLCORI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE RLCORU (IRET)
C-----------------------------------------------------------------------
C   RLCORU sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Input in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48
      INTEGER   IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM, BO, VO,
     *   NUMVIS, XCOUNT, NCORO, NCOPY, CATMP(256), RNXRET
      LOGICAL   T, F
      INCLUDE 'RLCOR.INC'
      REAL      VIS(UVBFSS), RESULT(UVBFSS), RPARM(20)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (BUFF1, VIS)
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      CALL UVPGET (IRET)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
      NUMVIS = 0
      XCOUNT = 0
C                                       make an index table
      CALL RNXGET (DISKIN, OLDCNO, CATOLD)
      CALL RNXINI (DISKO, NEWCNO, CATBLK, RNXRET)
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
C                                       call user routine
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
         CALL RLFIX (NUMVIS, RPARM, VIS, RESULT, IRET)
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1.0D0
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
C                                       update NX table
            CALL RNXUPD (RPARM, RNXRET)
C                                       Compressed
            IF (ISCOMP) THEN
               CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *            BUFF2(IPTRO+NRPRMO))
            ELSE
               CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
               END IF
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            END IF
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
C                                       close NX table
      IRET = 0
      CALL RNXCLS (RNXRET)
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
 1000 FORMAT ('RLCORU: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1010 FORMAT ('RLCORU: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('RLCORU: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('RLCORU: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('RLCORU: RLFIX ERROR',I3)
 1150 FORMAT ('RLCORU: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE RLCORH
C-----------------------------------------------------------------------
C   RLCORH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I, NONOT, J, IFNO(2)
      INCLUDE 'RLCOR.INC'
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
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRBUF, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       CLCORPRM
      J = 0
      DO 10 I = LBIF,LEIF
         J = J + 1
         WRITE (HILINE,1010) TSKNAM, I, CLCORP(J), I
         CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
 10      CONTINUE
C                                       Close HI file
 100  CALL HICLOS (LUN2, .TRUE., IBUFF2, IERR)
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'RLCORH: ERROR COPYING TABLES TO OUTPUT UV'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, NEWCNO, DIFPIX, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'RLCORH: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                       fix PCAL solutions
      IFNO(1) = LBIF
      IFNO(2) = LEIF
      CALL ANTCOR (DISKO, NEWCNO, ISUBAR, CATBLK, IFNO, CLCORP, IBUFF1,
     *   IFQID, IERR)
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RLCORH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'CLCORPRM(',I2.2,') = ',F8.3,5X,'/ R-L correction',
     *   ' in deg for IF ',I2)
      END
      SUBROUTINE RLFIX (NUMVIS, RPARM, VIS, RESULT, IRET)
C-----------------------------------------------------------------------
C   RLFIX corrects phase of up to 20 IFs in the desired subarray and
C   FQID.
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      NRPARM     I       # random parameters.
C      NCOR       I       # correlators
C      CATBLK     I(256)  Catalog header record. See Going Aips for
C                         details.
C      NRPRMI     I    Input number of random parameters.
C      INCSI      I    Input Stokes' increment in vis.
C      INCFI      I    Input frequency increment in vis.
C      INCIFI     I    Input IF increment in vis.
C      LRECO      I    Output file record length
C      NRPRMO     I    Output number of random parameters.
C      INCSO      I    Output Stokes' increment in vis.
C      INCFO      I    Output frequency increment in vis.
C      INCIFO     I    Output IF increment in vis.
C   Output:
C      RESULT     R(3,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      VIS(3,*), RPARM(*), RESULT(3,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI, IA1, IA2,
     *   JSUB, JFQ
      REAL      BASEN, VR, VI, GR, GI
      INCLUDE 'RLCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
C                                       pointers to traverse the data
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            IA1 = BASEN / 256. + 0.1
            IA2 = BASEN - IA1*256. + 0.1
            JSUB = 100.0 * (BASEN - 256*IA1 - IA2) + 1.01
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            JSUB = RPARM(1+ILOCSA) + 0.1
            END IF
         JFQ = 1
         IF (ILOCFQ.GE.0) JFQ = RPARM(1+ILOCFQ) + 0.01
         DO 40 JIF = 1,NIF
            DO 30 JF = 1,NF
               DO 20 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
                  RESULT(1,INDEXO) = VIS(1,INDEXI)
                  RESULT(2,INDEXO) = VIS(2,INDEXI)
                  RESULT(3,INDEXO) = VIS(3,INDEXI)
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
C                                       correction
         IF ((NS.EQ.4) .AND. (JSUB.EQ.ISUBAR) .AND. (JFQ.EQ.IFQID)) THEN
            DO 80 JIF = LBIF,LEIF
               GR = PARM(1,JIF-LBIF+1)
               GI = -PARM(2,JIF-LBIF+1)
               DO 70 JF = 1,NF
                  DO 60 JS = 3,4
                     INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *                  (JS-1) * INCSO + 1
                     VR = RESULT(1,INDEXO)
                     VI = RESULT(2,INDEXO)
                     GI = -GI
                     RESULT(1,INDEXO) = VR*GR - VI*GI
                     RESULT(2,INDEXO) = VI*GR + VR*GI
 60               CONTINUE
 70            CONTINUE
 80         CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE ANTCOR (DISK, CNO, INVER, CATBLK, IFNO, PCOR, BUFFER,
     *   FREQID, IERR)
C-----------------------------------------------------------------------
C   ANTCOR corrects the left hand polarization solutions by a specified
C   amount.  The correction depends on the polarization solution type
C   indicated by the table header keyword 'POLTYPE'.
C   Inputs:
C      DISK      I      Volume number
C      CNO       I      Catalog slot number
C      INVER     I      Input version number (subarray number)
C      CATBLK(*) I      Catalog header block
C      IFNO(2)   I      Range of IFs; 0 => 1.
C      PCOR(*)   R      Phase offsets of IFs in degrees
C      BUFFER(*) I      I/O Buffer
C      FREQID    I      FQ ID user wishes to change
C   Output:
C      IERR      I      Error code, 0=OK, >0 failed, <0 not PCAL found
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, INVER, CATBLK(256), IFNO(2), BUFFER(*),
     *   FREQID, IERR
      REAL      PCOR(*)
C
      CHARACTER    CHPOLT*8, CHSOL(4)*8, SOLTYP*8
      INTEGER   IIF, LUN, IANT, INDEX, LOCS, KEYTYP, ISTYPE, BIF, EIF,
     *   NXIF, NUMREC, IREF, MSGSAV
      REAL      POLP1, POLP2, SPCOR, CPCOR
      HOLLERITH XSOLTY(2)
      INCLUDE 'INCS:PUVD.INC'
      REAL      PD(MAXIF)
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CHSOL /'ORI-ELP ', 'APPROX  ', 'X-Y LIN', 'VLBI'/
      DATA CHPOLT /'POLTYPE '/
      DATA LUN /28/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
C                                      Open AN extension file.
      CALL ANTINI ('WRIT', BUFFER, DISK, CNO, INVER, CATBLK, LUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRIT'
         GO TO 990
         END IF
C                                       Check FREQID compatibility.
      IF ((ANFQID.GT.0) .AND. (FREQID.GT.0) .AND. (ANFQID.NE.FREQID)
     *   .AND. (ANAME.NE.'ATCA') .AND. (ANAME.NE.'ATLBA') .AND.
     *   (ANAME.NE.'LBA')) THEN
         MSGTXT = 'WARNING - POTENTIALLY FATAL ERROR'
         CALL MSGWRT (4)
         MSGTXT = '   The polarization variables in your AN table have'
         CALL MSGWRT (4)
         WRITE (MSGTXT,1030) ANFQID
         CALL MSGWRT (4)
         WRITE (MSGTXT,1040) FREQID
         CALL MSGWRT (4)
         MSGTXT = '   Are you sure this is what you want to do?'
         CALL MSGWRT (4)
         END IF
      NUMREC = BUFFER(5)
C                                       IF range to modify.
C                                       This is a risky to tell the
C                                       number of IFs.
      NXIF = ANNUMV(9) / 2
      BIF = IFNO(1)
      IF (BIF.GT.NXIF) BIF = NXIF
      IF (BIF.LE.0) BIF = 1
      EIF = IFNO(2)
      IF (EIF.GT.NXIF) EIF = NXIF
      IF (EIF.LE.0) EIF = 1
C                                       Check solution type keyword.
      MSGSUP = 32000
      CALL TABKEY ('READ', CHPOLT, 1, BUFFER, LOCS, XSOLTY, KEYTYP,
     *   IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         IF (IERR.GT.20) THEN
            MSGTXT = 'WARNING: NO PCAL SOLUTION FOUND, ' //
     *         'SO NONE CORRECTED'
            IERR = -1
         ELSE
            WRITE (MSGTXT,1060) IERR
            END IF
         GO TO 990
         END IF
C                                       Decide solution type:
      ISTYPE = 0
      CALL H2CHR (8, 1, XSOLTY, SOLTYP)
      IF (SOLTYP.EQ.CHSOL(1)) ISTYPE = 1
      IF (SOLTYP.EQ.CHSOL(2)) ISTYPE = 2
      IF (SOLTYP.EQ.CHSOL(3)) ISTYPE = 3
      IF (SOLTYP.EQ.CHSOL(4)) ISTYPE = 4
C                                       Unknown pol. solution type.
      IF (ISTYPE.EQ.0) THEN
         MSGTXT = 'WARNING: PCAL SOLUTION UNKNOWN, SO NOT CORRECTED'
         CALL MSGWRT (6)
         IERR = -1
         WRITE (MSGTXT,1070) SOLTYP
         GO TO 990
         END IF
      MSGTXT = 'Correcting polarization solutions in AN table'
      CALL MSGWRT (2)
C                                       For ISTYPE=1 (ORI-ELP) only need
C                                       to modify R-L phase differences
      IF (SOLTYP.EQ.'ORI-ELP ') THEN
C                                       Close AN table
         CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1200) IERR
            GO TO 990
            END IF
C                                       Fetch old phase differences
         CALL PDRGET (DISK, CNO, INVER, LUN, CATBLK, NXIF, IREF, PD,
     *      BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Update values (radians)
         INDEX = 1
         DO 80 IIF = BIF,EIF
            PD(IIF) = PD(IIF) + PCOR(INDEX) * 1.745329E-2
            INDEX = INDEX + 1
 80         CONTINUE
C                                       Save results
         CALL PDRSET (DISK, CNO, INVER, LUN, CATBLK, NXIF, IREF, PD,
     *      BUFFER, IERR)
C                                       Done
         GO TO 999
         END IF
C                                       Read AN records
      DO 200 IANT = 1,NUMREC
         IANRNO = IANT
         CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'READ'
            GO TO 990
            END IF
C                                       Feed polarizations
         INDEX = 1 + (BIF-1) * 2
         DO 150 IIF = BIF,EIF
C                                       Make appropriate correction
            IF ((ISTYPE.GE.2) .AND. (ISTYPE.LE.4)) THEN
C                                       Linear approximation
               CPCOR = COS (PCOR(IIF-BIF+1) * 1.745329E-2)
               SPCOR = SIN (PCOR(IIF-BIF+1) * 1.745329E-2)
C                                       Right hand (or X) parameters
               POLP1 = POLCA(INDEX)
               POLP2 = POLCA(INDEX+1)
               POLCA(INDEX) = POLP1 * CPCOR - POLP2 * SPCOR
               POLCA(INDEX+1) = POLP2 * CPCOR + POLP1 * SPCOR
C                                       Left hand (or Y) parameters
               POLP1 = POLCB(INDEX)
               POLP2 = POLCB(INDEX+1)
               POLCB(INDEX) = POLP1 * CPCOR + POLP2 * SPCOR
               POLCB(INDEX+1) = POLP2 * CPCOR - POLP1 * SPCOR
C                                       Nothing for now
            ELSE
               CONTINUE
               END IF
            INDEX = INDEX + 2
 150        CONTINUE
C                                       Write record
         IANRNO = IANT
         CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV,
     *      ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.EQ.0) GO TO 200
            WRITE (MSGTXT,1100) IERR, 'WRIT'
            GO TO 990
 200     CONTINUE
C                                      Close AN extension files
      CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1200) IERR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANTCOR: ERROR',I3,' OPEN-FOR-',A4,'ING AN FILE')
 1030 FORMAT ('   previously been modified for FQID ',I3)
 1040 FORMAT ('   You are now changing them with FREQID =',I3)
 1060 FORMAT ('ANTCOR: ERROR',I3,' FINDING POL. SOLUTION TYPE KEYWORD')
 1070 FORMAT ('ANTCOR: UNKNOWN POLN. SOLN. TYPE = ',A8)
 1100 FORMAT ('ANTCOR: ERROR',I3,1X,A4,'ING AN FILE')
 1200 FORMAT ('ANTCOR: ERROR',I3,' CLOSING AN FILE')
      END
