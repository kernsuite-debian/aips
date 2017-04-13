LOCAL INCLUDE 'VBCAL.INC'
C                                       Local include for VBCAL
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ, INCSI,
     *   INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI, LRECO, NRPRMI,
     *   NRPRMO, CATOLD(256), ILOCWT
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, HISCRD(20)*64
      REAL   XSIN, XDISIN, XSOUT, XDISO, APARM(10), BPARM(90), DOWEI,
     *   XCENT, BUFF1(UVBFSS), BUFF2(UVBFSS), DIFPIX
      DOUBLE PRECISION UVSCAL
      LOGICAL   ISCOMP
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INPTS/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, APARM, BPARM, DOWEI, XCENT
      COMMON /VBCALP/ UVSCAL, CATOLD, SEQIN, SEQOUT, DISKIN, DISKO,
     *   INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI, LRECO,
     *   NRPRMI, NRPRMO, ISCOMP, NUMHIS, ILOCWT, DIFPIX
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, HISCRD
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM VBCAL
C-----------------------------------------------------------------------
C! Applies antenna based amplitude gain factors.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000-2001, 2008-2009, 2012, 2014-2015
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
C   VBCAL applies antenna based amplitude calibration factors
C   to a uv data set.  Data to be calibrated can be selected by
C   antenna, IF, array and time range. Up to 90 antennas can be
C   calibrated at once.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      APARM(10)      APARM         Control info
C                                   1-4 => start day, hr, min, sec.
C                                   5-8 => End day, hr, min, sec.
C                                          0's => all.
C                                   9   => first ant. no + (array-1)*100
C                                       0=all first subarray,
C                                       .lt. 0=> all subarrays.
C                                   10  => 0 for RCP, 1 for LCP.,2=both.
C      BPARM(90)      BPARM         1-90 => antenna cal. factor.
C                                        baseline factor=SQRT(fac1*fac2)
C                                        0 => 1.0
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'VBCAL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'VBCAL '/
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      TSKNAM = PRGM
C                                       Get input parameters and
C                                       create output file if nec.
      IRET = 0
      CALL VBCLIN (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL VBCLUV (IRET)
      IF (IRET.EQ.0) CALL VBCLHI
C                                       Close down
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE VBCLIN (JERR)
C-----------------------------------------------------------------------
C   VBCLIN gets input parameters for VBCAL and creates an output file
C   if necessary.
C   Inputs:
C   See prologue comments in VBCAL for more details.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, BLANK*6, UTYPE*2
      INTEGER   JERR, OLDCNO, IROUND, NPARM, IERR, INCX
      INCLUDE 'VBCAL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK /' '/
C-----------------------------------------------------------------------
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 116
      CALL GTPARM (TSKNAM, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         JERR = 8
         RQUICK = .FALSE.
         GO TO 990
         END IF
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Restart AIPS.
      IERR = 0
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (6)
         END IF
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         JERR = 5
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         JERR = 5
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Find weight and scale.
      IF (ISCOMP) THEN
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
C                                       center frequencies
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.GE.0.0) THEN
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
         UVSCAL = CATD(KDCRV+JLOCF) / FREQ
      ELSE
         UVSCAL = 1.0D0
         DIFPIX = 0.0
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1060) IERR
            JERR = 8
            GO TO 990
         ELSE
C                                        Update existing CATBLK
            FRW(NCFILE+1) = 2
            CALL CATIO ('WRIT', DISKO, CCNO, CATBLK, 'WRIT', BUFF1,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1070) IERR
               CALL MSGWRT (6)
               END IF
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
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('VBCLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' RESTARTING AIPS')
 1015 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1020 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1070 FORMAT ('VBCLIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE VBCLUV (IRET)
C-----------------------------------------------------------------------
C   VBCLUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48
      INTEGER   IRET, INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO,
     *   ILENBU, KBIND, NIOLIM, IBIND, I, IA1, IA2, NIOUT, VO, BO,
     *   NUMVIS, XCOUNT, NCORI, NCORO, NCOPY
      LOGICAL   T, F
      INCLUDE 'VBCAL.INC'
      REAL      CBUFF(UVBFSS), RESULT(UVBFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA VO, BO /0,1/
      DATA LUNI, LUNO /16,17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, T, F, F, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
C                                       LRECO = length of output rec.
      LRECO = LREC
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
      NCORI = (LRECI - NRPRMI) / CATOLD(KINAX)
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
C                                       Loop Read vis. record.
 50   CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 990
      ELSE IF (INIO.GT.0) THEN
         IPTRI = IBIND
         DO 90 I = 1,INIO
            BUFF1(IPTRI+ILOCU) = BUFF1(IPTRI+ILOCU) * UVSCAL
            BUFF1(IPTRI+ILOCV) = BUFF1(IPTRI+ILOCV) * UVSCAL
            BUFF1(IPTRI+ILOCW) = BUFF1(IPTRI+ILOCW) * UVSCAL
            IF (ILOCB.GE.0) THEN
               IA1 = BUFF1(IPTRI+ILOCB) / 256. + 0.1
               IA2 = BUFF1(IPTRI+ILOCB) - IA1*256. + 0.1
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
C                                      Call user routine.
            IF (ISCOMP) THEN
C                                       Compressed data.
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL CALIB (NUMVIS, BUFF1(IPTRI+ILOCT), IA1, IA2, CBUFF,
     *            BUFF1(IPTRI), RESULT, IRET)
            ELSE
               CALL CALIB (NUMVIS, BUFF1(IPTRI+ILOCT), IA1, IA2,
     *            BUFF1(IPTRI+NRPARM), BUFF1(IPTRI), RESULT, IRET)
               END IF
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1060) IRET
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1
C                                       Copy to output.
               CALL RCOPY (NRPRMO, BUFF1(IPTRI), BUFF2(IPTRO))
               IF (ISCOMP) THEN
C                                       Compressed
                  CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
               ELSE
                  CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO))
                  END IF
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               END IF
            IPTRI = IPTRI+LREC
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1070) IRET
                  GO TO 990
                  END IF
               END IF
            IPTRO = KBIND
            NIOUT = 0
 90         CONTINUE
         GO TO 50
         END IF
C                                       Final call to CALIB.
      NUMVIS = -1
      CALL CALIB (NUMVIS, BUFF2(IPTRO+ILOCT), IA1, IA2,
     *   BUFF2(IPTRO+NRPARM), BUFF2(IPTRO), RESULT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1060) IRET
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1070) IRET
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
 1000 FORMAT ('VBCLUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('VBCLUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('VBCLUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1050 FORMAT ('VBCLUV: ERROR',I3,' READING VIS FILE')
 1060 FORMAT ('VBCLUV: CALIB ERROR',I3)
 1070 FORMAT ('VBCLUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE VBCLHI
C-----------------------------------------------------------------------
C   VBCLHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(1)*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IBUFF(512), NONOT, IERR, I
      LOGICAL   T
      INCLUDE 'VBCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (IBUFF,BUFF2)
      DATA LUN1, LUN2 /27,28/
      DATA  T /.TRUE./
      DATA NONOT, NOTTYP /0,'  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any user supplied history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1010) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
         END IF
C
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(2), FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, FCNO(1), DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'VBCLHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VBCLHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1020 FORMAT ('VBCLHI: ERROR COPYING TABLES')
      END
      SUBROUTINE CALIB (NUMVIS, T, IA1, IA2, VIS, RPARM, RESULT, IRET)
C-----------------------------------------------------------------------
C  CALIB does antenna based amplitude calibration using antenna
C  gain factors passed in BPARM.
C
C  Inputs:
C  NUMVIS     I    Visibility number, -1 => final call, no data
C                  passed but allows any operations to be completed.
C                  Data sent back will be wirtten to output file.
C  T          R    Time in days since 0 IAT on the first day for which
C                  there is data.
C  IA1        I    First antenna number
C  IA2        I    Second antenna number
C  RPARM(*)   I    Random parameter array which includes U,V,W etc
C                  but also any other random parameters.
C  VIS(3,*)   R    Visibilities in order real, imaginary, weight (Jy)
C
C  Inputs from COMMON
C  APARM(10)  R    Control info. See VBCAL comments for details.
C  BPARM(90)  R    Antenna calibration factors.
C  NRPARM     I    # random parameters.
C  NCOR       I    # correlators
C  CATBLK(256)I    Catalog header record. See [DOC]HEADER for details
C
C  Output:
C  RPARM      R    Modified random parameter array. NB U,V,W,
C                  time and baseline should not be modified in RPARM
C  VIS        R    Visibilities
C  IRET       I    Return code  -1 => don't write
C                                0 => OK
C                               >0 => error, terminate.
C
C  Output in COMMON
C  NUMHIS     I    # history entries (max. 10)
C  HISCRD(16,NUMHIS) R   History records
C  CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CHRL(3)*2
      INTEGER   NUMVIS, IA1, IA2, IRET, NS, NIF, NF, INDEXI, INDEXO,
     *   IAFRST, IP1(4), IP2(4), IRL, JRL, IARR, I, NFREQ, ISADD, JARR,
     *   IV, JP1, JP2, JIF, JF, JS
      REAL      T, RPARM(*), VIS(3,*), TFIRST, TLAST, TIME,
     *   CALIF(2,MAXANT), CALWT(2,MAXANT), FACTOR, TEMP, RESULT(3,*)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'VBCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      SAVE CALIF, IARR, IAFRST, TFIRST, TLAST, IRL, JRL, NFREQ, ISADD,
     *   NS, NIF, NF, CALWT
      DATA IP1 /1,2,1,2/
      DATA IP2 /1,2,2,1/
      DATA CHRL /'R ','L ','RL'/
C-----------------------------------------------------------------------
      IRET = 0
C                                        Construct table.
      IF (NUMVIS.EQ.1) THEN
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         IF (APARM(9).LT.0.0) THEN
            IARR = 0
            IAFRST = 1
         ELSE
            IARR = APARM(9)*0.01 + 1.01
            IAFRST = APARM(9) - (IARR-1)*100. + 0.01
            IF (IAFRST.LE.0) IAFRST = 1
            END IF
         TFIRST = APARM(1) + APARM(2)/24. + APARM(3)/(24.*60.) +
     *      APARM(4)/(24.*3600.)
         TLAST = APARM(5) + APARM(6)/24. + APARM(7)/(24.*60.) +
     *      APARM(8)/(24.*3600.)
         IF (TLAST.LE.TFIRST) APARM(5) = 300.
         IF (TLAST.LE.TFIRST) TLAST = 300.
         IRL = APARM(10) + 1.01
         IF (IRL.LE.0) IRL = 1
         IF (IRL.GT.2) IRL = 2
         JRL = IRL
         IF (APARM(10).GT.1.9) JRL = 3 - IRL
         CALL RFILL (2*MAXANT, 1.0, CALIF)
         CALL RFILL (2*MAXANT, 1.0, CALWT)
         IF (APARM(9).LE.0.0) THEN
            IF (BPARM(1).LE.0.0) BPARM(1) = 1.0
            TEMP = SQRT (BPARM(1))
            DO 10 I = 1,MAXANT
               CALIF(IRL,I) = TEMP
               CALIF(JRL,I) = TEMP
               CALWT(IRL,I) = 1.0 / BPARM(1)
               CALWT(JRL,I) = 1.0 / BPARM(1)
 10            CONTINUE
         ELSE
            DO 20 I = 1,90
               IF (BPARM(I).GT.0.0) THEN
                  CALIF(IRL,IAFRST+I-1) = SQRT (BPARM(I))
                  CALIF(JRL,IAFRST+I-1) = SQRT (BPARM(I))
                  CALWT(IRL,IAFRST+I-1) = 1.0 / BPARM(I)
                  CALWT(JRL,IAFRST+I-1) = 1.0 / BPARM(I)
                  END IF
 20            CONTINUE
            END IF
         IF (DOWEI.LE.0.0) CALL RFILL (2*MAXANT, 1.0, CALWT)
         NFREQ = CATBLK(KINAX+JLOCF)
         ISADD = ABS (ICOR0) - 1
         END IF
C                                        Process record.
      IF (NUMVIS.GE.1) THEN
C                                        Determine array.
         IF (ILOCB.GE.0) THEN
            JARR = RPARM(ILOCB+1) + 0.1
            JARR = 100. * (RPARM(ILOCB+1) - JARR) + 1.5
         ELSE
            JARR = RPARM(ILOCSA+1) + 0.1
            END IF
         IF ((JARR.NE.IARR) .AND. (IARR.GT.0)) GO TO 999
         TIME = T - 5. * (JARR - 1)
         IF ((TIME.LT.TFIRST).OR.(TIME.GT.TLAST)) GO TO 999
C                                        Loop thru frequency.
         DO 50 JIF = 1,NIF
            DO 40 JF = 1,NF
               DO 30 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
                  JP1 = IP1(JS+ISADD)
                  JP2 = IP2(JS+ISADD)
                  FACTOR = CALIF(JP1,IA1) * CALIF(JP2,IA2)
                  RESULT(1,INDEXO) = VIS(1,INDEXI) * FACTOR
                  RESULT(2,INDEXO) = VIS(2,INDEXI) * FACTOR
                  FACTOR = CALWT(JP1,IA1) * CALWT(JP2,IA2)
                  RESULT(3,IV) = VIS(3,INDEXI) * FACTOR
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
C                                         Write history.
      ELSE
         WRITE (HISCRD(1),2000) (APARM(I),I=1,8)
         IF (IRL.NE.JRL) IRL = 3
         WRITE (HISCRD(2),2001) IARR, IAFRST, CHRL(IRL)
C                                       All antenna calibration.
         IF (APARM(9).LE.0.0) THEN
            WRITE (HISCRD(3),2004) BPARM(1)
            NUMHIS = 3
         ELSE
            WRITE (HISCRD(3),2002) (BPARM(I),I=1,5)
            WRITE (HISCRD(4),2003) (BPARM(I),I=6,10)
            WRITE (HISCRD(5),2002) (BPARM(I),I=11,15)
            WRITE (HISCRD(6),2003) (BPARM(I),I=16,20)
            WRITE (HISCRD(7),2002) (BPARM(I),I=21,25)
            WRITE (HISCRD(8),2003) (BPARM(I),I=26,30)
            WRITE (HISCRD(9),2002) (BPARM(I),I=31,35)
            WRITE (HISCRD(10),2003) (BPARM(I),I=36,40)
            WRITE (HISCRD(11),2002) (BPARM(I),I=41,45)
            WRITE (HISCRD(12),2003) (BPARM(I),I=46,50)
            WRITE (HISCRD(13),2002) (BPARM(I),I=51,55)
            WRITE (HISCRD(14),2003) (BPARM(I),I=56,60)
            WRITE (HISCRD(15),2002) (BPARM(I),I=61,65)
            WRITE (HISCRD(16),2003) (BPARM(I),I=66,70)
            WRITE (HISCRD(17),2002) (BPARM(I),I=71,75)
            WRITE (HISCRD(18),2003) (BPARM(I),I=76,80)
            WRITE (HISCRD(19),2002) (BPARM(I),I=81,85)
            WRITE (HISCRD(20),2003) (BPARM(I),I=86,90)
            NUMHIS = 20
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 2000 FORMAT ('/ Timerange ',4F5.0,' to ',4F5.0)
 2001 FORMAT ('/ Array=',I3,' First ant.=',I3,' Polarization=',1A2)
 2002 FORMAT ('BPARM=',5(F10.5,','))
 2003 FORMAT (6X,5(F10.5,','))
 2004 FORMAT ('/ All antennas multiplied by ',F10.5)
      END
