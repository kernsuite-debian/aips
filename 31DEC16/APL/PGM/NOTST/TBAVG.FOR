LOCAL INCLUDE 'TBAVG.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2),  XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XDISO, XSOUT, SOLINT, SHIFT(2),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), FINC(MAXIF)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT, INCSI,
     *   INCFI, INCIFI, NRPRMI, NPOL, NIF, NCHAN, ISBAND(MAXIF)
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      DOUBLE PRECISION FOFF(MAXIF)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, SOLINT, SHIFT
      COMMON /TBAVGP/ FOFF, FINC, ISBAND, SEQIN, SEQOUT, DISKIN, DISKO,
     *   ILOCWT, ISCOMP, INCSI, INCFI, INCIFI, NRPRMI, NPOL, NIF, NCHAN
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
LOCAL END
      PROGRAM TBAVG
C-----------------------------------------------------------------------
C! Time averages data on all baselines.
C# Utility UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2008, 2012, 2015
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
C   TBAVG time averages data on all baselines together.
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
C      SOLINT         SOLINT        Averaging time.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'TBAVG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'TBAVG '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL TBAVIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL TBAVUV (IRET)
      IF (IRET.EQ.0) CALL TBAVHI
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE TBAVIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   TBAVIN gets input parameters for TBAVG and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      ISCOMP  L  If true data is compressed
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX, I, K, LUN, FRQSEL
      LOGICAL   T
      INCLUDE 'TBAVG.INC'
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /' '/
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
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Check SOLINT
      IF (SOLINT.LT.1) SOLINT = 1.0
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
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
C                                       Check that data in TB order
      IF (ISORT(1:1).NE.'T') THEN
         JERR = 8
         MSGTXT = 'DATA MUST BE IN TB ORDER'
         GO TO 990
         END IF
C                                       Save input file info
      INCX = CATBLK(KINAX)
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
      NRPRMI = NRPARM
      NPOL = CATBLK(KINAX+JLOCS)
      NCHAN = CATBLK(KINAX+JLOCF)
      NIF = 1
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
C                                       get freq info
      I = 1
      LUN = 28
      FRQSEL = 1
      CALL CHNDAT ('READ', BUFF1, DISKIN, OLDCNO, I, CATBLK, LUN, K,
     *   FOFF, ISBAND, FINC, BNDCOD, FRQSEL, JERR)
      IF (JERR.GT.0) THEN
         MSGTXT = 'ERROR READING FQ FILE'
         GO TO 990
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
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
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
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
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
 1000 FORMAT ('TBAVIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('TBAVIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE TBAVUV (IRET)
C-----------------------------------------------------------------------
C   TBAVUV sends uv data one point at a time to the averaging
C   routine and then writes the modified data if requested.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48, IFILE*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LRECO, BO,
     *   ILENBU, KBIND, NIOUT, NIOLIM, IBIND, I, VO, NUMVIS, XCOUNT,
     *   NCORI, NCORO, NCOPY, IBAS, IA1, IA2
      LOGICAL   T, F
      INCLUDE 'TBAVG.INC'
      REAL      CBUFF(UVBFSS)
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
C                                       LRECO = length of output rec.
      LRECO = LREC
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LREC - NRPARM) / CATBLK(KINAX)
      NCORO = (LRECO - NRPARM) / CATBLK(KINAX)
      NCOPY = LRECO - NRPARM
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
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
C                                       Loop
C                                       Read vis. record.
 100  CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
         END IF
      IPTRI = IBIND
C                                       Out of data?
      IF (INIO.GE.0) THEN
         IF (INIO.LE.0) BUFF1(IPTRI+ILOCT) = 10000.
C                                       Loop over buffer
         DO 190 I = 1,MAX(1,INIO)
            IF (INIO.GT.0) THEN
               IF (ILOCB.GE.0) THEN
                  IBAS = BUFF1(IPTRI+ILOCB) + 0.01
                  IA1 = IBAS / 256
                  IA2 = IBAS - IA1*256
               ELSE
                  IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
                  IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
                  END IF
C                                       forbid auto-correlations
               IF (IA1.NE.IA2) THEN
                  NUMVIS = NUMVIS + 1
                  IF (INIO.EQ.0) NUMVIS = -1
C                                       Call averaging routine.
C                                       Compressed data.
                  IF (ISCOMP) THEN
                     CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPARM),
     *                  BUFF1(IPTRI+ILOCWT), CBUFF)
                     CALL TBAVER (NUMVIS, BUFF1(IPTRI+ILOCU),
     *                  BUFF1(IPTRI+ILOCV), BUFF1(IPTRI+ILOCW),
     *                  BUFF1(IPTRI+ILOCT), CBUFF, BUFF1(IPTRI), IRET)
C                                       Un compressed data
                  ELSE
                     CALL TBAVER (NUMVIS, BUFF1(IPTRI+ILOCU),
     *                  BUFF1(IPTRI+ILOCV), BUFF1(IPTRI+ILOCW),
     *                  BUFF1(IPTRI+ILOCT), BUFF1(IPTRI+NRPARM),
     *                  BUFF1(IPTRI), IRET)
                     END IF
C                                       Branch on his return
C                                       Error (fatal)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1120) IRET
                     GO TO 990
C                                       Copy to output.
                  ELSE IF (IRET.EQ.0) THEN
                     XCOUNT = XCOUNT + 1.0D0
C                                       Compressed
                     IF (ISCOMP) THEN
                        CALL RCOPY (NRPARM, BUFF1(IPTRI), BUFF2(IPTRO))
                        CALL ZUVPAK (NCORO, CBUFF, BUFF2(IPTRO+ILOCWT),
     *                     BUFF2(IPTRO+NRPARM))
                     ELSE
                        CALL RCOPY (LREC, BUFF1(IPTRI), BUFF2(IPTRO))
                        END IF
                     IPTRO = IPTRO + LRECO
                     NIOUT = NIOUT + 1
                     END IF
C                                       Write vis record.
                  IF (NIOUT.GE.NIOLIM) THEN
                     CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM,
     *                  KBIND, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1150) IRET
                        GO TO 990
                        END IF
                     IPTRO = KBIND
                     NIOUT = 0
                     END IF
                  END IF
               IPTRI = IPTRI + LREC
               END IF
 190        CONTINUE
C                                       Read next buffer.
         IF (INIO.GT.0) GO TO 100
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
 1000 FORMAT ('TBAVUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('TBAVUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('TBAVUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('TBAVUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('TBAVUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('TBAVUV: TBAVER ERROR',I3)
 1150 FORMAT ('TBAVUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE TBAVHI
C-----------------------------------------------------------------------
C   TBAVHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(9)*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, NONOT
      LOGICAL   T
      INCLUDE 'TBAVG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /9, 'NX','CL','SN','BP','FG','IM','PC','MC',
     *   'BL'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       SOLINT
      WRITE (HILINE,2000) TSKNAM, SOLINT
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       SOLINT
      WRITE (HILINE,2010) TSKNAM, SHIFT
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TBAVHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1200 FORMAT ('TBAVHI: ERROR COPYING TABLES')
 2000 FORMAT (A6,'SOLINT =',F10.3,' / Averaging time (sec)')
 2010 FORMAT (A6,'SHIFT =',F10.5,' ,',F10.5,5X,'/ Shift in asec at ref')
      END
      SUBROUTINE TBAVER (NUMVIS, U, V, W, T, VIS, RPARM, IRET)
C-----------------------------------------------------------------------
C   Routine to average data.
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      U       R    U in wavelengths
C      V       R    V in wavelengths
C      W       R    W in wavelengths
C      T       R    Time in days since 0 IAT on the first day for
C                   which there is data, the julian day corresponding
C                   to this day can be obtained in D   form by:
C                   CALL JULDAY (CATH(KHDOB),XDAY) where XDAY will
C                   be the Julian day number.
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      SOLINT     R       Averaging time in seconds.
C   Output:
C      U          R    U in wavelengths
C      V          R    V in wavelengths
C      W          R    W in wavelengths
C      T          R    Time in same units as input.
C      RPARM      R    Modified random parameter array.
C      VIS        R    Visibilities
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      U, V, W, T, VIS(3,*), RPARM(*)
C
      INCLUDE 'TBAVG.INC'
      INTEGER   MVIS, LOOP, TIMCNT, IIF, IC, IP
      LOGICAL   DOSHIF
      REAL      TIMSUM, TIMLST, TIMINC, TEMP1, TEMP2, TEMP3, NORM,
     *   SAVIS(3,MAXCIF), XX
      DOUBLE PRECISION RA0, DEC0, DXC, DYC, DZC, RFREQ, DFREQ, CFREQ,
     *   TRUEF
      COMPLEX   VS , ZZ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE MVIS, SAVIS, TIMSUM, TIMLST, TIMINC, TIMCNT, DXC, DYC, DZC,
     *   RFREQ, DOSHIF
C-----------------------------------------------------------------------
C                                       First call -setup
      IF (NUMVIS.EQ.1) THEN
C                                       zero accumulators
         MVIS = NPOL * NIF * NCHAN
         DO 50 LOOP = 1,MVIS
            SAVIS(1,LOOP) = 0.0
            SAVIS(2,LOOP) = 0.0
            SAVIS(3,LOOP) = 0.0
 50         CONTINUE
C                                       TIMINC = averaging time in days.
         TIMINC = (SOLINT-0.1) / (24.0 * 60.0 * 60.0)
C                                       TIMLST = last time in current
C                                       average.
         TIMLST = T + TIMINC
C                                       TIMCNT = Number of time samples.
         TIMCNT = 0
C                                       TIMSUM = sum of times.
         TIMSUM = 0.0
C                                       shift
         DOSHIF = (SHIFT(1).NE.0.0) .OR. (SHIFT(2).NE.0.0)
         IF (DOSHIF) THEN
            RFREQ = CATR(KRCRP+JLOCF)
            RA0 = RA
            DEC0 = DEC
            IF (COS(DG2RAD*DEC0).NE.0.0D0) RA = RA0 + SHIFT(1) / 3600.D0
     *         / COS(DG2RAD * DEC0)
            DEC = DEC + SHIFT(2) / 3600.D0
            DXC = SIN (DG2RAD * (RA-RA0)) * COS (DEC * DG2RAD)
            DYC = COS (DEC0 * DG2RAD) * SIN (DEC * DG2RAD) -
     *         SIN (DEC0 * DG2RAD) * COS (DEC * DG2RAD) *
     *         COS ((RA - RA0) * DG2RAD)
            DZC = SIN (DG2RAD * DEC0) * SIN (DG2RAD * DEC) +
     *         COS (DG2RAD * DEC0) * COS (DG2RAD * DEC) *
     *         COS (DG2RAD * (RA - RA0)) - 1.0D0
            DXC = TWOPI * DXC
            DYC = TWOPI * DYC
            DZC = TWOPI * DZC
            END IF
         END IF
C                                       New average?
      IF ((NUMVIS.LT.0) .OR. (T.GT.TIMLST)) THEN
         DO 120 IIF = 1,NIF
            TRUEF = 1.0D0 + FOFF(IIF) / FREQ
            DFREQ = FINC(IIF) / FREQ
            DO 110 IP = 1,NPOL
               DO 100 IC = 1,NCHAN
                  LOOP = (IP-1) * INCSI + (IC-1) * INCFI + (IIF-1)
     *               * INCIFI + 1
C                                       average
                  IF (SAVIS(3,LOOP).GT.0.0) THEN
                     NORM = 1.0 / SAVIS(3,LOOP)
                     TEMP1 = SAVIS(1,LOOP) * NORM
                     TEMP2 = SAVIS(2,LOOP) * NORM
                     TEMP3 = SAVIS(3,LOOP)
                  ELSE
                     TEMP1 = 0.0
                     TEMP2 = 1.E-20
                     TEMP3 = 0.0
                     END IF
C                                       start new sums
                  IF (VIS(3,LOOP).GT.0.0) THEN
                     VS = CMPLX (VIS(1,LOOP), VIS(2,LOOP))
                     IF (DOSHIF) THEN
                        CFREQ = TRUEF + DFREQ * (IC - RFREQ)
                        XX = (U * DXC + V * DYC + W * DZC) * CFREQ
                        ZZ = CMPLX (COS(XX), -SIN(XX))
                        VS = VS * ZZ
                        END IF
                     SAVIS(1,LOOP) = REAL (VS) * VIS(3,LOOP)
                     SAVIS(2,LOOP) = AIMAG (VS) * VIS(3,LOOP)
                     SAVIS(3,LOOP) = VIS(3,LOOP)
                  ELSE
                     SAVIS(1,LOOP) = 0.0
                     SAVIS(2,LOOP) = 0.0
                     SAVIS(3,LOOP) = 0.0
                     END IF
C                                       return average
                  VIS(1,LOOP) = TEMP1
                  VIS(2,LOOP) = TEMP2
                  VIS(3,LOOP) = TEMP3
 100              CONTINUE
 110           CONTINUE
 120        CONTINUE
C                                       Reset last time
         TIMLST = T + TIMINC
C                                       Baseline = 1-2
         IF (ILOCB.GE.0) THEN
            RPARM(1+ILOCB) = 258.0
         ELSE
            RPARM(1+ILOCA1) = 1
            RPARM(1+ILOCA2) = 2
            END IF
C                                       Time
         IF (TIMCNT.GT.0) THEN
            TIMSUM = TIMSUM / TIMCNT
         ELSE
            TIMSUM = T - TIMINC * 0.5
            END IF
         RPARM(1+ILOCT) = TIMSUM
C                                       Set u,v,w to zero
         RPARM(1+ILOCU) = 0.0
         RPARM(1+ILOCV) = 0.0
         RPARM(1+ILOCW) = 0.0
         TIMSUM = T
         TIMCNT = 1
C                                       Write datum
         IRET = 0
C                                       Accumulate
      ELSE
         DO 220 IIF = 1,NIF
            TRUEF = 1.0D0 + FOFF(IIF) / FREQ
            DFREQ = FINC(IIF) / FREQ
            DO 210 IP = 1,NPOL
               DO 200 IC = 1,NCHAN
                  LOOP = (IP-1) * INCSI + (IC-1) * INCFI + (IIF-1)
     *               * INCIFI + 1
                  IF (VIS(3,LOOP).GT.0.0) THEN
                     VS = CMPLX (VIS(1,LOOP), VIS(2,LOOP))
                     IF (DOSHIF) THEN
                        CFREQ = TRUEF + DFREQ * (IC - RFREQ)
                        XX = (U * DXC + V * DYC + W * DZC) * CFREQ
                        ZZ = CMPLX (COS(XX), -SIN(XX))
                        VS = VS * ZZ
                        END IF
                     SAVIS(1,LOOP) = REAL (VS) * VIS(3,LOOP) +
     *                  SAVIS(1,LOOP)
                     SAVIS(2,LOOP) = AIMAG (VS) * VIS(3,LOOP) +
     *                  SAVIS(2,LOOP)
                     SAVIS(3,LOOP) = VIS(3,LOOP) + SAVIS(3,LOOP)
                     END IF
 200              CONTINUE
 210           CONTINUE
 220        CONTINUE
         TIMSUM = TIMSUM + T
         TIMCNT = TIMCNT + 1
C                                       Normally don't write data.
         IRET = -1
         END IF
C
 999  RETURN
      END
