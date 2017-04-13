LOCAL INCLUDE 'AVER.INC'
C                                       Local include for AVER
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   CATBLK(256), CATIN(256), SEQIN, SEQOUT, DISKIN, DISKO,
     *   NUMHIS, JBUFSZ
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      REAL      XSIN, XDISIN, XSOUT, XDISO, YINC, XCENT, BUFF1(UVBFSS),
     *   BUFF2(UVBFSS), DIFPIX
      CHARACTER HISCRD(10)*64
      DOUBLE PRECISION UVSCAL
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ, NUMHIS
      COMMON /OLDHDR/ CATIN
      COMMON /INPTS/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, YINC, XCENT
      COMMON /AVERP/ UVSCAL, SEQIN, SEQOUT, DISKIN, DISKO, DIFPIX
      COMMON /CHRCOM/ HISCRD, NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /MAPHDR/ CATBLK
LOCAL END
      PROGRAM AVER
C-----------------------------------------------------------------------
C! Time average 'BT' sorted uv data.
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000, 2008-2009, 2014-2015
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
C   AVER averages a uv data set to a maximum time given by YINC.
C   Data must initially be in baseline-time ('BT') order.
C   Weights are summed and other values are averaged without weighting.
C   Inputs:
C   AIPS adverb  Prg. name.          Description.
C   INNAME         NAMEIN        Name of input UV data.
C   INCLASS        CLAIN         Class of input UV data.
C   INSEQ          SEQIN         Seq. of input UV data.
C   INDISK         DISKIN        Disk number of input VU data.
C   OUTNAME        NAMOUT        Name of the output uv file.
C   OUTCLASS       CLAOUT        Class of the output uv file.
C   OUTSEQ         SEQOUT        Seq. number of output uv data.
C   OUTDISK        DISKO         Disk number of the output file.
C   YINC           YINC          Integration time (sec) min=0.2
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'AVER.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'AVER  '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL AVERIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Average data.
      CALL AVERUV (IRET)
      IF (IRET.EQ.0) CALL AVERHI
C                                       close down
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE AVERIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   AVERIN gets input parameters for AVER and creates an output file
C   if necessary.
C   Inputs:  PRGM   C*6      Task name
C   Output:  JERR   I        Error code: quit if > 0.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGM*6, BLANK*6, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   OLDCNO, IROUND, JERR, NPARM, IERR, INCX
      REAL      CATR(256)
      LOGICAL   T
      DOUBLE PRECISION CATD(128)
      INCLUDE 'AVER.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATBLK, CATH, CATD)
      DATA BLANK /'      '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 16
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
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
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      YINC = MAX (10.0, YINC) / 86400.0
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Copy old CATBLK
      CALL COPY (256, CATBLK, CATIN)
C                                       Disallow packed uv data
      IF (CATBLK(KINAX).EQ.1) THEN
         JERR = 9
         MSGTXT = 'ERROR: I cannot process packed UV data, use SPLIT'
         GO TO 990
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Check if sort order 'BT'
      IF (ISORT.NE.'BT') THEN
         WRITE (MSGTXT,1050) ISORT
         JERR = 5
         GO TO 990
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
      IF (JLOCF.LT.0) XCENT = -1.0
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
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 70
         IF (IERR.EQ.2) GO TO 60
            WRITE (MSGTXT,1055) IERR
            GO TO 990
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
 60      IF ((CCNO.EQ.OLDCNO) .AND. (DISKO.EQ.DISKIN)) GO TO 65
            WRITE (MSGTXT,1060)
            GO TO 990
C                                       Recover existing CATBLK
 65      CONTINUE
            FRW(NCFILE+1) = 2
            CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1,
     *         IERR)
            IF (IERR.EQ.0) GO TO 70
               WRITE (MSGTXT,1065) IERR
               CALL MSGWRT (6)
 70   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                        Put input file in READ
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      SEQOUT = CATBLK(KIIMS)
      JERR = 0
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AVERIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('SORT ORDER ',A2,' NOT BT AS REQUIRED')
 1055 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('AVERIN: ERROR',I3,' UPDATING NEW CATBLK')
 4000 FORMAT ('You are using a non-standard program')
      END
      SUBROUTINE AVERUV (IRET)
C-----------------------------------------------------------------------
C   AVERUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET, INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LRECO,
     *   ILENBU, KBIND, IBIND, NIOUT, NIOLIM, I, VO, BO, NUMVIS, XCOUNT,
     *   IA1, IA2
      REAL      BASEL
      CHARACTER IFILE*48, OFILE*48
      LOGICAL   T, F
      INCLUDE 'AVER.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA VO, BO /0, 1/
      DATA LUNI, LUNO /16,17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Open vis file for write
 10   CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 20
         WRITE (MSGTXT,1010) IRET
         GO TO 990
C                                       Init vis file for write
C                                       LRECO = length of output rec.
 20   LRECO = LREC
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         GO TO 990
 30   IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IRET
         GO TO 990
 40   NUMVIS = 0
      XCOUNT = 0
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.EQ.0) GO TO 110
            WRITE (MSGTXT,1100) IRET
            GO TO 990
 110     IPTRI = IBIND
         DO 190 I = 1,INIO
            NUMVIS = NUMVIS + 1
            IF (ILOCB.GE.0) THEN
               BASEL = BUFF1(IPTRI+ILOCB)
               IA1 = BASEL / 256. + 0.1
               IA2 = BASEL - IA1*256. + 0.1
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
            BUFF1(IPTRI+ILOCU) = BUFF1(IPTRI+ILOCU) * UVSCAL
            BUFF1(IPTRI+ILOCV) = BUFF1(IPTRI+ILOCV) * UVSCAL
            BUFF1(IPTRI+ILOCW) = BUFF1(IPTRI+ILOCW) * UVSCAL
C                                      Average
            CALL AVERDO (NUMVIS, BUFF1(IPTRI+ILOCU),
     *         BUFF1(IPTRI+ILOCV), BUFF1(IPTRI+ILOCW),
     *         BUFF1(IPTRI+ILOCT), BUFF1(IPTRI+NRPARM),
     *         BUFF1(IPTRI), IRET)
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1
               CALL RCOPY (LREC, BUFF1(IPTRI), BUFF2(IPTRO))
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               END IF
            IPTRI = IPTRI + LREC
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
         IF (INIO.GT.0) GO TO 100
C                                       Final call to AVERDO.
      NUMVIS = -1
      CALL AVERDO (NUMVIS, BUFF2(IPTRO+ILOCU), BUFF2(IPTRO+ILOCV),
     *   BUFF2(IPTRO+ILOCW), BUFF2(IPTRO+ILOCT), BUFF2(IPTRO+NRPARM),
     *   BUFF2(IPTRO), IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
      ELSE IF (IRET.EQ.0) THEN
         XCOUNT = XCOUNT + 1
         IF (ILOCB.GE.0) THEN
            BUFF2(IPTRO+ILOCB) = BASEL
         ELSE
            BUFF2(IPTRO+ILOCA1) = IA1
            BUFF2(IPTRO+ILOCA2) = IA2
            END IF
         IPTRO = IPTRO + LRECO
         NIOUT = NIOUT + 1
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
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AVERUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1010 FORMAT ('AVERUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1020 FORMAT ('AVERUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('AVERUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('AVERUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('AVERUV: AVERDO ERROR',I3)
 1150 FORMAT ('AVERUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE AVERHI
C-----------------------------------------------------------------------
C   AVERHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(1)*2, LABEL*8, HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'AVER.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2, T /27,28, .TRUE./
      DATA NONOT, NOTTYP /1,'NX'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any user supplied history.
      IF (NUMHIS.LE.0) GO TO 20
         WRITE (LABEL,1010) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, FCNO(1), DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'AVERHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AVERHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1020 FORMAT ('AVERHI: ERROR',I3,' COPYING TABLES')
      END
      SUBROUTINE AVERDO (NUMVIS, U, V, W, T, VIS, RPARM, IRET)
C-----------------------------------------------------------------------
C   AVERDO averages a uv data set in time.
C   Inputs:
C      NUMVIS     I    Visibility number, -1 => final call, no data
C                      passed but allows any operations to be completed.
C                      Data sent back will be wirtten to output file.
C      U          R    U in wavelengths
C      V          R    V in wavelengths
C      W          R    W in wavelengths
C      T          R    Time in days since 0 IAT on the first day for
C                      which there is data.
C      RPARM(*)   I    Random parameter array which includes U,V,W etc
C                      but also any other random parameters.
C      VIS(3,*)   R    Vis data in order real, imaginary, weight (Jy)
C   Inputs from COMMON
C      NRPARM     I    # random parameters.
C      NCOR       I    # correlators
C      CATBLK(256)I    Catalog header record.
C   Output:
C      RPARM      R    Modified random parameter array. NB U,V,W, time
C                      and baseline should not be modified in RPARM
C      VIS        R    Visibilities
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C   Output in COMMON
C      NUMHIS     I    # history entries (max. 10)
C      HISCRD(NUMHIS) C*64   History records
C      CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      U, V, W, T, VIS(3,*), RPARM(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LUNSS, JERR, INDEX, IDAY, IWT(1024), IIWT, MCOR, NMCOR,
     *   I, J, XCOUNT, ANVER, IA1CUR, IA2CUR, ISUCUR, JA1, JA2, JSUB
      REAL      VBUFF(4*MAXCIF), TEMP, CATR(128), TCHK, CT, TLAST, UT,
     *   VT, WT, TT, USUM, VSUM, WSUM, TSUM, DIV
      DOUBLE PRECISION CATD(128), X8
      INCLUDE 'AVER.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      SAVE IA1CUR, IA2CUR, ISUCUR, TCHK, TLAST, USUM, VSUM, WSUM, TSUM,
     *   VBUFF, IIWT
      DATA LUNSS /27/
C-----------------------------------------------------------------------
      IRET = -1
C                                        Initial call
      IF (NUMVIS-1) 300, 100, 200
 100     XCOUNT = 0
C                                        Get IAT - UTC
         ANVER = 1
         CALL ANTINI ('READ', VBUFF, FVOL(NCFILE), FCNO(NCFILE), ANVER,
     *      CATIN, LUNSS, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0,
     *      DEGPDY, SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS,
     *      ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, JERR)
         IF (JERR.EQ.0) CALL TABAN ('CLOS', VBUFF, IANRNO, ANKOLS,
     *      ANNUMV, ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *      DIAMAN, FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB,
     *      JERR)
         CT = T - DATUTC
         TCHK = CT
         IDAY = CT
         X8 = (CT-IDAY) / YINC
         TLAST = IDAY + DINT (X8) * YINC + YINC
C                                        First vis., copy to buffer.
         MCOR = (LREC - NRPARM) / 3
         NMCOR = (LREC - NRPARM)
C                                        Zero sums.
         USUM = 0.0
         VSUM = 0.0
         WSUM = 0.0
         TSUM = 0.0
         CALL RFILL (NMCOR, 0.0, VBUFF)
         CALL FILL (MCOR, 0, IWT)
         IIWT = 0
         IF (ILOCB.GE.0) THEN
            UT = RPARM(ILOCB+1)
            IA1CUR = UT / 256.0 + 0.1
            IA2CUR = UT - 256 * IA1CUR + 0.1
            ISUCUR = (UT - 256 * IA1CUR - IA2CUR) * 100.0 + 1.1
         ELSE
            IA1CUR = RPARM(ILOCA1+1) + 0.1
            IA2CUR = RPARM(ILOCA2+1) + 0.1
            ISUCUR = RPARM(ILOCSA+1) + 0.1
            END IF
C                                        Check that LREC not too big
         IF (LREC.GT.3100) THEN
            WRITE (MSGTXT,1110)
            CALL MSGWRT (8)
            IRET = 8
            GO TO 999
            END IF
C                                        Next datum.
 200     IF (ILOCB.GE.0) THEN
            UT = RPARM(ILOCB+1)
            JA1 = UT / 256.0 + 0.1
            JA2 = UT - 256 * JA1 + 0.1
            JSUB = (UT - 256 * JA1 - JA2) * 100.0 + 1.1
         ELSE
            JA1 = RPARM(ILOCA1+1) + 0.1
            JA2 = RPARM(ILOCA2+1) + 0.1
            JSUB = RPARM(ILOCSA+1) + 0.1
            END IF
         UT = U
         VT = V
         WT = W
         TT = T
         CT = T - DATUTC
C                                        Check time and baseline.
         IF ((CT.GT.TLAST) .OR. (JA1.NE.IA1CUR). OR. (JA2.NE.IA2CUR)
     *      .OR. (JSUB.NE.ISUCUR) .OR. (CT.LT.TCHK)) GO TO 250
C                                        Sum
            USUM = USUM + UT
            VSUM = VSUM + VT
            WSUM = WSUM + WT
            TSUM = TSUM + TT
            IIWT = IIWT + 1
            DO 220 I = 1,MCOR
               INDEX = (I-1) * 3 + 1
C                                        Check weight.
               IF (VIS(3,I).LE.0.0) GO TO 220
                  VBUFF(INDEX) = VBUFF(INDEX) + VIS(1,I)
                  VBUFF(INDEX+1) = VBUFF(INDEX+1) + VIS(2,I)
                  VBUFF(INDEX+2) = VBUFF(INDEX+2) + VIS(3,I)
                  IWT(I) = IWT(I) + 1
 220           CONTINUE
            TCHK = CT
            GO TO 999
C                                        "New" integration - write.
 250     CONTINUE
            XCOUNT = XCOUNT + 1
            DO 270 I = 1,MCOR
               DO 260 J = 1,3
                  INDEX = (I-1) * 3 + J
                  TEMP = VIS(J,I)
                  IF (VIS(3,I).LE.0.0) TEMP = 0.0
                  DIV = 1.0
                  IF (J.NE.3) DIV = 1.0 / MAX (1, IWT(I))
                  VIS(J,I) = VBUFF(INDEX) * DIV
                  IF ((J.EQ.3) .AND. (VIS(J,I).LE.0.0)) VIS(J,I) = -1.0
                  VBUFF(INDEX) = TEMP
 260              CONTINUE
               IWT(I) = 0
               IF (TEMP.GT.0.0) IWT(I) = 1
 270           CONTINUE
            U = USUM / MAX (1, IIWT)
            V = VSUM / MAX (1, IIWT)
            W = WSUM / MAX (1, IIWT)
            T = TSUM / MAX (1, IIWT)
            IIWT = 1
            USUM = UT
            VSUM = VT
            WSUM = WT
            TSUM = TT
            IF (ILOCB.GT.0) THEN
               RPARM(ILOCB+1) = 256*IA1CUR + IA2CUR + (ISUCUR-1)/100.0
            ELSE
               RPARM(ILOCA1+1) = IA1CUR
               RPARM(ILOCA2+1) = IA2CUR
               RPARM(ILOCSA+1) = ISUCUR
               END IF
            IA1CUR = JA1
            IA2CUR = JA2
            ISUCUR = JSUB
            IDAY = CT
            X8 = (CT-IDAY) / YINC
            TLAST = IDAY + DINT(X8) * YINC + YINC
            TCHK = CT
            IRET = 0
            GO TO 999
C                                         Last datum.
 300  CONTINUE
            XCOUNT = XCOUNT + 1
            DO 320 I = 1,MCOR
               DO 310 J = 1,3
                  INDEX = (I-1) * 3 + J
                  DIV = 1.0
                  IF (J.NE.3) DIV = 1.0 / MAX (1, IWT(I))
                  VIS(J,I) = VBUFF(INDEX) * DIV
                  IF ((J.EQ.3) .AND. (VIS(J,I).LE.0.0)) VIS(J,I) = -1.0
 310              CONTINUE
 320           CONTINUE
            U = USUM / MAX (1, IIWT)
            V = VSUM / MAX (1, IIWT)
            W = WSUM / MAX (1, IIWT)
            T = TSUM / MAX (1, IIWT)
C                                        Write no. written to history.
            NUMHIS = 2
            YINC = YINC * 86400.0
            WRITE (HISCRD(1),1998) YINC
            WRITE (HISCRD(2),1999) XCOUNT
            WRITE (MSGTXT,1999) XCOUNT
            CALL MSGWRT (5)
            IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1110 FORMAT ('RECORD LENGTH=',I5,' GREATER THAN 1000')
 1998 FORMAT ('Average time=',F7.2,' sec.')
 1999 FORMAT (I10,' Visibility records written')
      END
