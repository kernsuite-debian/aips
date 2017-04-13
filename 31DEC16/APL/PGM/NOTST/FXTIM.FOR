LOCAL INCLUDE 'FXTIM.INC'
C                                       Local include for FXTIM
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XREFDA(2)
      REAL      XSIN, XDISIN, XSOUT, XDISO, XCENT,
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, TOFF, IBUFF1(UVBFSS), IBUFF2(UVBFSS)
      LOGICAL   ISCOMP
      DOUBLE PRECISION UVSCAL
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, REFDAY*8
      EQUIVALENCE (IBUFF1, BUFF2), (IBUFF2, BUFF2)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XREFDA, XCENT
      COMMON /OTPARM/ CATOLD, UVSCAL, SEQIN, SEQOUT, DISKIN, DISKO,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, ISCOMP, TOFF, DIFPIX
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, REFDAY
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
C                                       End local include for FXTIM
LOCAL END
      PROGRAM FXTIM
C-----------------------------------------------------------------------
C! Fixes times in a uv data set
C# Utility UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2000, 2007-2008, 2014-2015
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
C   FXTIM fixes the times in a uv data set by determining the minimum
C   and adding a sutiable number of days.
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
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'FXTIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'FXTIM '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FXTIMI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL FXTIMU (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL FXTIMH
C                                       Close down files, etc.
 990  CALL DIE (IRET, IBUFF1)
C
 999  STOP
      END
      SUBROUTINE FXTIMI (PRGN, JERR)
C-----------------------------------------------------------------------
C   FXTIMI gets input parameters for FXTIM and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      LRECI   I  Input file record length
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
C   See prologue comments in FXTIM for more details.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX
      LOGICAL   T
      INCLUDE 'FXTIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /'      '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 17
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, IBUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (8, 1, XREFDA, REFDAY)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
      IF (ISCOMP) THEN
C                                       Find weight and scale.
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
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
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
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
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, IBUFF1, IERR)
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
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', IBUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
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
     *   PTYPE, NLUSER, 'READ', IBUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FXTIMI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('FXTIMI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE FXTIMU (IRET)
C-----------------------------------------------------------------------
C   FXTIMU finds the minimum time and then copies the data adding an
C   integer number of days.
C   Input in common:
C      LRECI   I  Input file record length
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
      CHARACTER OFILE*48, IFILE*48, OBSDAT*8
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, ILENBU,
     *   KBIND, NIOUT, NIOLIM, IBIND, I, IA1, IA2, INCX, BO, VO,
     *   NUMVIS, XCOUNT, NCORI, NCORO, NCOPY
      LOGICAL   T, F
      DOUBLE PRECISION JD, JDR
      INCLUDE 'FXTIM.INC'
      REAL      DUM, BASEN, CBUFF(UVBFSS), RESULT(UVBFSS), TMIN, TMAX
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
      IF (ISCOMP) INCX = 3
      TMIN = 1.E20
      TMAX = -TMIN
C                                       REFDATE used?
      IF (REFDAY.NE.' ') THEN
         CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
         CALL JULDAY (OBSDAT, JD)
         CALL JULDAY (REFDAY, JDR)
         TMIN = JDR - JD + 0.000001D0
         END IF
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LRECI - NRPRMI) / CATOLD(KINAX)
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'READ'
         GO TO 990
         END IF
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'READ'
         GO TO 990
         END IF
C                                       Loop
 10   CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ'
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       Loop over buffer
         IF (INIO.GT.0) THEN
            DO 20 I = 1,INIO
               TMIN = MIN (TMIN, BUFF1(IPTRI+ILOCT))
               TMAX = MAX (TMAX, BUFF1(IPTRI+ILOCT))
               IPTRI = IPTRI + LRECI
 20            CONTINUE
            GO TO 10
            END IF
C                                       have extreme times
      WRITE (MSGTXT,1020) TMIN, TMAX
      CALL MSGWRT (3)
C                                       no correction
      IF (TMIN.GE.0.0) THEN
         TOFF = 0
      ELSE
         TOFF = -TMIN + 0.99999
         CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
         CALL JULDAY (OBSDAT, JD)
         JD = JD - TOFF
         CALL GREG (JD, OBSDAT)
         CALL CHR2H (8, OBSDAT, 1, CATH(KHDOB))
         END IF
      WRITE (MSGTXT,1025) TOFF
      CALL MSGWRT (3)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'WRITE'
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'WRITE'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'READ'
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ'
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       Loop over buffer
         DO 190 I = 1,INIO
            IF (ILOCB.GE.0) THEN
               BASEN = BUFF1(IPTRI+ILOCB)
               IA1 = BASEN / 256. + 0.1
               IA2 = BASEN - IA1*256. + 0.1
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
            BUFF1(IPTRI+ILOCU) = BUFF1(IPTRI+ILOCU) * UVSCAL
            BUFF1(IPTRI+ILOCV) = BUFF1(IPTRI+ILOCV) * UVSCAL
            BUFF1(IPTRI+ILOCW) = BUFF1(IPTRI+ILOCW) * UVSCAL
            NUMVIS = NUMVIS + 1
C                                      Call user routine.
            IF (ISCOMP) THEN
C                                       Compressed data.
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL DIDDLE (NUMVIS, BUFF1(IPTRI+ILOCT), CBUFF,
     *            INCX, RESULT, IRET)
            ELSE
C                                       Un compressed data
               CALL DIDDLE (NUMVIS, BUFF1(IPTRI+ILOCT),
     *            BUFF1(IPTRI+NRPRMI), INCX, RESULT, IRET)
               END IF
C                                       Branch on his return
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1.0D0
               CALL RCOPY (NRPRMO, BUFF1(IPTRI), BUFF2(IPTRO))
C                                       Compressed
               IF (ISCOMP) THEN
                  CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
               ELSE
                  CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
                  END IF
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               END IF
C                                       OK, but no output please
            IPTRI = IPTRI + LRECI
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) IRET, 'WRITE'
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
C                                       Read next buffer.
         IF (INIO.GT.0) GO TO 100
C                                       Final call to DIDDLE.
      NUMVIS = -1
      CALL DIDDLE (NUMVIS, DUM, BUFF1, INCX, RESULT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'FLUSH'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FXTIMU: ERROR',I3,1X,A,' FOR ',A,' VIS FILE')
 1010 FORMAT ('FXTIMU: ERROR',I3,1X,A,' VIS FILE')
 1020 FORMAT ('FXTIMU: TMIN=',F9.3,'  TMAX',F9.3)
 1025 FORMAT ('FXTIMU: times corrected by adding',I6,' days')
 1120 FORMAT ('FXTIMU: DIDDLE ERROR',I3)
      END
      SUBROUTINE FXTIMH
C-----------------------------------------------------------------------
C   FXTIMH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, NONOT
      LOGICAL   T
      INCLUDE 'FXTIM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /0, '  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      IF (REFDAY.NE.' ') THEN
         WRITE (HILINE,1005) TSKNAM, REFDAY
         CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      WRITE (HILINE,1010) TSKNAM, TOFF
      CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, IBUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(2), FCNO(1), CATBLK, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'FXTIMH: ERROR COPYING TABLES'
         CALL MSGWRT (6)
         END IF
C                                       correct times
      CALL DTIMTA (LUN1, LUN2, DISKO, FCNO(1), TOFF, IBUFF1, IBUFF2,
     *   IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'FXTIMH: ERROR CHANGING TIMES IN TABLES'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, FCNO(1), DIFPIX, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'FXTIMH: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   IBUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FXTIMH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1005 FORMAT (A6,'REFDATE =''',A,'''   / new reference date forced')
 1010 FORMAT (A6,'TOFFSET =',I8, ' / Days added to time')
      END
      SUBROUTINE DIDDLE (NUMVIS, T, VIS, INCX, RESULT, IRET)
C-----------------------------------------------------------------------
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      T       R    Time in days since 0 IAT on the first day for
C                   which there is data, the julian day corresponding
C                   to this day can be obtained in D   form by:
C                   CALL JULDAY (CATH(KHDOB),XDAY) where XDAY will
C                   be the Julian day number.
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      RA         D       Right ascension (1950) of phase center. (deg)
C      DEC        D       Declination (1950) of phase center. (deg)
C      FREQ       D       Frequency of observation (Hz)
C      NRPARM     I       # random parameters.
C      NCOR       I       # correlators
C      CATBLK     I(256)  Catalog header record. See Going Aips for
C                         details.
C      LRECI      I    Input file record length
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
C      T          R    Time in same units as input.
C      RESULT  R(INCX,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, INCX, IRET
      REAL      T, VIS(INCX,*), RESULT(INCX,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI, I
      INCLUDE 'FXTIM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         T = T + TOFF
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         DO 40 JIF = 1,NIF
            DO 30 JF = 1,NF
               DO 20 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
                  DO 10 I = 1,INCX
                     RESULT(I,INDEXO) = VIS(I,INDEXI)
 10                  CONTINUE
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE DTIMTA (LUN1, LUN2, VOL, CNO, TOFF, BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C   DTIMTA re-writes tables correcting the times if any.
C   Inputs:
C      LUN1     I        LUN for read
C      LUN2     I        LUN for write
C      VOL      I        Disk number
C      CNO      I        Catalog slot number
C      TOFF     I        Time increment in days
C   Output:
C      BUFF1    I(512)   Work buffer
C      BUFF2    I(512)   Work buffer
C      IRET     I        Return error code  0 => ok
C                           1 => files the same, no copy.
C                           2 => no input files exist
C                           3 => failed
C                           4 => no output files created.
C                           5 => failed to update CATBLK
C-----------------------------------------------------------------------
      INTEGER   LUN1, LUN2, VOL, CNO, TOFF, BUFF1(*), BUFF2(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TIMKEY(2)*24, TYPE*2
      REAL      RECR(XBPRSZ)
      DOUBLE PRECISION RECD(XBPRSZ/2)
      INTEGER   NVERS, NKEY, NREC, NCOL, DATP(128,2,2), IER, I, TIMKOL,
     *   RECORD(XBPRSZ), NRECIN, INREC, OUTREC, LOOP, INVER,
     *   OUTVER, ITYPE, NT, LENGTH, RTYPE
      LOGICAL   T, F, TABLE, EXIST, FITASC, OPEN1, OPEN2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (RECORD, RECR, RECD)
      DATA TIMKEY /'TIME', 'TIME RANGE'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       loop over types
      DO 100 ITYPE = 1,KIEXTN
         CALL H2CHR (2, 1, CATH(KHEXT+ITYPE-1), TYPE)
         NVERS = CATBLK(KIVER+ITYPE-1)
         DO 90 INVER = 1,NVERS
            OPEN1 = F
            OPEN2 = F
            EXIST = .FALSE.
            IRET = 0
            IF ((TYPE.NE.'HI') .AND. (TYPE.NE.'PL') .AND. (TYPE.NE.'SL')
     *         .AND. (TYPE.NE.' ') .AND. (TYPE.NE.'SU')) CALL ISTAB
     *         (TYPE, VOL, CNO, INVER,LUN1, BUFF1, TABLE, EXIST, FITASC,
     *         IRET)
            IF ((IRET.EQ.0) .AND. (EXIST) .AND. (TABLE)) THEN
C                                       Open first (input) file
               CALL TABINI ('READ', TYPE, VOL, CNO, INVER, CATBLK, LUN1,
     *            NKEY, NREC, NCOL, DATP(1,1,1), BUFF1, IER)
C                                       If not there - quit
               IF (IER.GT.0) GO TO 90
               OPEN1 = T
C                                       Find source id column.
               NT = 0
               CALL FNDCOL (1, TIMKEY(I), 8, T, BUFF1, TIMKOL, IER)
               IF (IER.EQ.0) THEN
                  NT = 1
               ELSE
                  CALL FNDCOL (1, TIMKEY(2), 12, T, BUFF1, TIMKOL, IER)
                  IF (IER.EQ.0) NT = 2
                  END IF
C                                       quit if no source column
               IF (NT.GT.0) THEN
C                                       Open second (output) file
                  OUTVER = INVER
                  CALL TABINI ('WRIT', TYPE, VOL, CNO, OUTVER, CATBLK,
     *               LUN2, NKEY, NREC, NCOL, DATP(1,1,2), BUFF2, IER)
                  IF (IER.GT.0) GO TO 80
                  OPEN2 = T
C                                       Get number of records in the
C                                       files.
                  NRECIN = BUFF1(5)
                  OUTREC = 1
C                                       translate to physical place in
C                                       record
                  LENGTH = DATP(TIMKOL,2,1) / 10
                  RTYPE = DATP(TIMKOL,2,1) - LENGTH * 10
                  TIMKOL = DATP(TIMKOL,1,1)
C                                       Copy
                  DO 40 LOOP = 1,NRECIN
                     INREC = LOOP
                     CALL TABIO ('READ', 0, INREC, RECORD, BUFF1, IER)
                     IF (IER.GT.0) THEN
                        WRITE (MSGTXT,1070) IER, 'READ', TYPE
                        GO TO 990
                        END IF
C                                       time translate
                     DO 20 I = 1,NT
                        IF (RTYPE.EQ.1) THEN
                           RECD(TIMKOL+I-1) = RECD(TIMKOL+I-1) + TOFF
                        ELSE
                           RECR(TIMKOL+I-1) = RECR(TIMKOL+I-1) + TOFF
                           END IF
 20                     CONTINUE
                     CALL TABIO ('WRIT', 0, OUTREC, RECORD, BUFF2, IER)
                     IF (IER.GT.0) THEN
                        WRITE (MSGTXT,1070) IER, 'WRIT', TYPE
                        GO TO 990
                        END IF
                     OUTREC = OUTREC + 1
 40                  CONTINUE
                  END IF
C                                       Close tables.
 80            IF (OPEN1) THEN
                  CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
                  IF (IER.GT.0) THEN
                     WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
                     GO TO 990
                     END IF
                  END IF
               OPEN1 = F
               IF (OPEN2) THEN
                  CALL TABIO ('CLOS', 0, OUTREC, RECORD, BUFF2, IER)
                  IF (IER.GT.0) THEN
                     WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
                     GO TO 990
                     END IF
                  END IF
               END IF
 90         CONTINUE
 100     CONTINUE
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
      IF (OPEN1) CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
      IF (OPEN2) CALL TABIO ('CLOS', 0, OUTREC, RECORD, BUFF2, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('DSOUTA: ERROR ',I3,2X,A4,'ING ',A2,' TABLE')
      END
