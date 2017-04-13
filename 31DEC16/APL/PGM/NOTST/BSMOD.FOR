LOCAL INCLUDE 'BSMOD.INC'
C                                       Local include for BSMOD
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMOU(3), XCLAOU(2), XOPTYP, XDATE(2), XOBJ(2)
      REAL      XSOUT, XDISO, XGAUSS, GMAX(4), GPOS(2,4), GWID(3,4),
     *   CPARM(10), APARM(10), FLUX, BPARM(10), BUFF1(UVBFSS),
     *   BUFF2(UVBFSS), DFJ(4,2), DFN(4,2), DMAX(4,2), DDX(4,2),
     *   DDC(4,2), DDY(4,2), THROW, PCATR(256)
      INTEGER   SEQO1, DISKO, JBUFSZ, INCSO, INCFO, INCIFO, LRECO,
     *   NRPRMO, NGAUSS, PCAT(256), MCAT(256)
      LOGICAL   ISCOMP, ISFREQ, FIXRA, FIXDEC
      CHARACTER NAMOUT*12, CLAOUT*6, OPTYPE*4, REFDAT*8, OBJECT*8
      DOUBLE PRECISION PCATD(128)
      EQUIVALENCE (PCAT, PCATR, PCATD)
      COMMON /INPARM/ XNAMOU, XCLAOU, XSOUT, XDISO, XOPTYP, XGAUSS,
     *   GMAX, GPOS, GWID, CPARM, APARM, FLUX, BPARM, XDATE, XOBJ
      COMMON /SDMODC/ PCAT, MCAT, SEQO1, DISKO, INCSO, INCFO, INCIFO,
     *   LRECO, NRPRMO, ISCOMP, NGAUSS, DFJ, DFN, DMAX, DDX, DDC, DDY,
     *   ISFREQ, THROW, FIXRA, FIXDEC
      COMMON /CHARPM/ NAMOUT, CLAOUT, OPTYPE, REFDAT, OBJECT
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
C                                       End local include for BSMOD
LOCAL END
      PROGRAM BSMOD
C-----------------------------------------------------------------------
C! Models beam-switched continuum data
C# Modeling singledish
C-----------------------------------------------------------------------
C;  Copyright (C) 1999-2000, 2009, 2015
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
C   BSMOD adds a model to single-dish uv data or replaces the data with
C   the model plus noise.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQO1         Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      NGAUSS         XGAUSS        Number gaussian components
C      GMAX           GMAX          Peak value
C      GPOS           GPOS          Ra, dec offsets
C      GWIDTH         GWIDTH        major, minor, pa
C      FLUX           FLUX          < 0 -> add model in to data
C                                   >= 0 replace data with model +
C                                   noise w rms = FLUX
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'BSMOD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRGM /'BSMOD '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL SDMOIN (PRGM, IRET)
C                                       Call routine that alters data
      IF (IRET.EQ.0) CALL SDMOUV (IRET)
C                                       history
      IF (IRET.EQ.0) CALL SDMOHI
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SDMOIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   SDMOIN gets input parameters for BSMOD and creates output UV files
C   Inputs:
C      PRGN    C*6     Program name
C   Output:
C      IRET    I       Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
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
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER BLANK*12, CTYPE(5)*8, RTYPE(6)*8, CTEMP*8
      INTEGER   I, J, IROUND, NPARM, IERR, INCX, LOCS, KEYTYP
      REAL      OBST(4)
      LOGICAL   T
      INCLUDE 'BSMOD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA BLANK  /' '/
      DATA T /.TRUE./
      DATA RTYPE /'AZ---REL', 'EL---REL', 'TIME1', 'BEAM', 'SCAN',
     *   'SAMPLE'/
      DATA CTYPE /'COMPLEX', 'STOKES', 'FREQ', 'RA', 'DEC'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 68
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMOU, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (8, 1, XDATE, REFDAT)
      CALL H2CHR (8, 1, XOBJ, OBJECT)
      IF (NAMOUT.EQ.' ') NAMOUT = OBJECT
      IF (REFDAT.EQ.' ') REFDAT = '20000101'
      SEQO1 = IROUND (XSOUT)
      DISKO = IROUND (XDISO)
      NGAUSS = XGAUSS + 0.5
      IF (NGAUSS.LT.1) THEN
         MSGTXT = 'MODEL REQUIRED'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IF (BPARM(1).LE.0.0) THEN
         MSGTXT = 'BPARM(1)=THROW REQUIRED'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IF (ABS(BPARM(2)).LE.0.1) BPARM(2) = 1
      IF (BPARM(5).LE.0.1) BPARM(5) = 1
      IF (BPARM(6).LE.0.1) BPARM(6) = 1
C                                       Create new file from scratch
      CALL CATINI (CATBLK)
      CATBLK(KIPCN) = 6
      DO 10 I = 1,6
         J = KHPTP + (I-1) * 2
         CALL CHR2H (8, RTYPE(I), 1, CATH(J))
 10      CONTINUE
      CATBLK(KIDIM) = 5
      DO 20 I = 1,5
         J = KHCTP + (I-1) * 2
         CALL CHR2H (8, CTYPE(I), 1, CATH(J))
 20      CONTINUE
      CATBLK(KINAX) = 3
      CATBLK(KINAX+2) = 3
      CATD(KDCRV) = 1.0D0
      CATD(KDCRV+1) = 1.0D0
C                                       Miscellaneous items.
C                                       Sort order ('**'=>unsorted)
      CALL CHR2H (2, '  ', 1, CATH(KITYP))
C                                       Units
      CTEMP = 'K'
      CALL CHR2H (8, CTEMP, 1, CATH(KHBUN))
      CTEMP = REFDAT
      CALL CHR2H (8, CTEMP, 1, CATH(KHDOB))
      IF (CPARM(6).LT.1.0) CPARM(6) = 12.0
      WRITE (CTEMP,1020) CPARM(6)
      CALL CHR2H (8, CTEMP, 1, CATH(KHTEL))
      CALL CHR2H (8, OBJECT, 1, CATH(KHOBJ))
      CATR(KREPO) = 2000.0
C                                       Put new values in CATBLK.
      CLAOUT(6:6) = '+'
      CALL MAKOUT (BLANK, BLANK, 0, BLANK, NAMOUT, CLAOUT, SEQO1)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQO1
      CATR(KRBMJ) = APARM(7) / 3600.0
      CATR(KRBMN) = APARM(7) / 3600.0
      CATD(KDCRV+2) = RAD2DG * VELITE * 1.2 / CPARM(6) / CATR(KRBMJ)
      CATD(KDCRV+3) = CPARM(4)
      CATD(KDCRV+3) = CPARM(5)
      CATD(KDORA) = CPARM(4)
      CATD(KDODE) = CPARM(5)
      IF (CPARM(1).LE.0.0) CPARM(1) = 0.25
      IF (CPARM(2).LE.0.0) CPARM(2) = 60.0
      CPARM(3) = CPARM(3) * 15.0
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      IRET = 4
      CATBLK(KIGCN) = APARM(1) * APARM(4) + 0.1
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATING OUTPUT FILE'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
      CALL RCOPY (4, CPARM(6), OBST)
      CALL OTFANT (CCNO, DISKO, OBST)
C                                       Get throw
      THROW = BPARM(1) + APARM(2) / 4.0
      KEYTYP = 2
      LOCS = 1
      CALL CATKEY ('WRIT', DISKO, CCNO, 'BSTHROW', 1, LOCS, THROW,
     *   KEYTYP, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRITING THROW'
         GO TO 990
         END IF
C                                       Save output file info
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
      SEQO1 = CATBLK(KIIMS)
      CALL COPY (256, CATBLK, PCAT)
C                                       Put new values in CATBLK.
      CLAOUT(6:6) = '-'
      CALL MAKOUT (BLANK, BLANK, 0, BLANK, NAMOUT, CLAOUT, SEQO1)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQO1
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      IRET = 4
      CATBLK(KIGCN) = APARM(1) * APARM(4) + 0.1
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATING OUTPUT FILE'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
      CALL OTFANT (CCNO, DISKO, CPARM(6))
      CALL COPY (256, CATBLK, MCAT)
C                                       Get throw
      THROW = -BPARM(1) - APARM(2) / 4.0
      CALL CATKEY ('WRIT', DISKO, CCNO, 'BSTHROW', 1, LOCS, THROW,
     *   KEYTYP, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRITING THROW'
         GO TO 990
         END IF
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDMOIN: ERROR',I3,1X,A)
 1020 FORMAT ('BS',F5.1,'M')
      END
      SUBROUTINE SDMOUV (IRET)
C-----------------------------------------------------------------------
C   SDMOUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Input in common:
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
      CHARACTER OFILE1*48, OFILE2*48, CHSCAN*8, CHSAMP*8
      INTEGER   IPTRO, LUNM, LUNP, INDM, INDP, ILENBU, MBIND, NIOUT,
     *   NIOLIM, PBIND, BO, VO, NUMVIS, XCOUNT, NCORO, NCOPY, IU, IERR,
     *   IV, NU, NV, ILOCSC, ILOCSM
      LOGICAL   T, F
      INCLUDE 'BSMOD.INC'
      REAL      DUM, RESULT(4), U, V, TIME, DAY
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNM, LUNP /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
      DATA CHSCAN, CHSAMP /'SCAN','SAMP'/
C-----------------------------------------------------------------------
C                                       SCAN pointer
      CALL AXEFND (4, CHSCAN, CATBLK(KIPCN), CATH(KHPTP), ILOCSC, IERR)
      IF (IERR.NE.0) ILOCSC = -1
C                                       SAMPLE pointer
      CALL AXEFND (4, CHSAMP, CATBLK(KIPCN), CATH(KHPTP), ILOCSM, IERR)
      IF (IERR.NE.0) ILOCSM = -1
      IERR = 0
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO) / PCAT(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', FVOL(NCFILE), FCNO(NCFILE), 1, OFILE2, IRET)
      CALL ZOPEN (LUNM, INDM, FVOL(NCFILE), OFILE2, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', FVOL(NCFILE-1), FCNO(NCFILE-1), 1, OFILE1,
     *   IRET)
      CALL ZOPEN (LUNP, INDP, FVOL(NCFILE-1), OFILE1, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNP, INDP, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, MBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = PBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNM, INDM, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF1, BO, MBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
      NV = APARM(4) + 0.1
      NU = APARM(1) + 0.1
      DAY = 24.0 * 3600.0
      TIME = -CPARM(2) / DAY
C                                       Loop
      IPTRO = 1
      DO 100 IV = 1,NV
         TIME = TIME + CPARM(2)/DAY
         DO 90 IU = 1,NU
            U = (IU - NU/2 - 1) * APARM(2) / 3600.
            V = (IV - NV/2 - 1) * APARM(5) / 3600.
            CALL RFILL (LRECO, 0.0, BUFF1(IPTRO))
            IF (ILOCSC.GE.0) BUFF1(IPTRO+ILOCSC) = IV
            IF (ILOCSM.GE.0) BUFF1(IPTRO+ILOCSM) = IU
            BUFF1(IPTRO+ILOCT) = TIME
            TIME = TIME + CPARM(1)/DAY
            NUMVIS = NUMVIS + 1
            CALL BSMOMO (NUMVIS, U, V, TIME, RESULT, IRET)
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               BUFF1(IPTRO+ILOCT) = TIME
               IF (ILOCB.GE.0) THEN
                  BUFF1(IPTRO+ILOCB) = 257.0
               ELSE
                  BUFF1(IPTRO+ILOCA1) = 1.0
                  BUFF1(IPTRO+ILOCA2) = 1.0
                  END IF
               BUFF1(IPTRO+ILOCU) = U
               BUFF1(IPTRO+ILOCV) = V
               XCOUNT = XCOUNT + 1.0D0
               CALL RCOPY (NRPRMO, BUFF1(IPTRO), BUFF2(IPTRO))
               BUFF1(IPTRO+NRPRMO) = RESULT(1)
               BUFF2(IPTRO+NRPRMO) = RESULT(2)
               BUFF1(IPTRO+NRPRMO+2) = 1.0
               BUFF2(IPTRO+NRPRMO+2) = 1.0
               BUFF1(IPTRO+NRPRMO+3) = RESULT(3)
               BUFF2(IPTRO+NRPRMO+3) = RESULT(3)
               BUFF1(IPTRO+NRPRMO+5) = 1.0
               BUFF2(IPTRO+NRPRMO+5) = 1.0
               BUFF1(IPTRO+NRPRMO+6) = RESULT(4)
               BUFF2(IPTRO+NRPRMO+6) = RESULT(4)
               BUFF1(IPTRO+NRPRMO+8) = 1.0
               BUFF2(IPTRO+NRPRMO+8) = 1.0
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               END IF
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNP, INDP, BUFF2, NIOLIM, PBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
                  END IF
               CALL UVDISK ('WRIT', LUNM, INDM, BUFF1, NIOLIM, MBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
                  END IF
               IPTRO = MBIND
               NIOUT = 0
               END IF
 90         CONTINUE
 100     CONTINUE
C                                       Final call to BSMOMO.
      NUMVIS = -1
      CALL BSMOMO (NUMVIS, DUM, DUM, DUM, RESULT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      IPTRO = NIOUT
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNP, INDP, BUFF2, NIOUT, PBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
      NIOUT = - IPTRO
      CALL UVDISK ('FLSH', LUNM, INDM, BUFF1, NIOUT, MBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Close files
      CALL ZCLOSE (LUNM, INDM, IRET)
      CALL ZCLOSE (LUNP, INDP, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDMOUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('SDMOUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('SDMOUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('SDMOUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1120 FORMAT ('SDMOUV: BSMOMO ERROR',I3)
 1150 FORMAT ('SDMOUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE SDMOHI
C-----------------------------------------------------------------------
C   SDMOHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, LUN2, IERR, I
      REAL      XJ, XN
      LOGICAL   T
      INCLUDE 'BSMOD.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HICREA (LUN1, FVOL(NCFILE-1), FCNO(NCFILE-1), PCAT, BUFF1,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
      CALL HICREA (LUN2, FVOL(NCFILE), FCNO(NCFILE), MCAT, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQO1, FVOL(NCFILE), LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQO1, FVOL(NCFILE-1), LUN1,
     *   BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1012) TSKNAM, FLUX
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       components
      IF (NGAUSS.GT.0) THEN
         WRITE (HILINE,1014) TSKNAM, NGAUSS
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         CALL HIADD (LUN1, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
         HILINE = TSKNAM // '/ Pre-convolution Gaussian parameters:'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         DO 20 I = 1,NGAUSS
            WRITE (HILINE,1015) TSKNAM, I, GMAX(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            CALL HIADD (LUN1, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
            WRITE (HILINE,1016) TSKNAM, I, GPOS(1,I), GPOS(2,I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            CALL HIADD (LUN1, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
            WRITE (HILINE,1017) TSKNAM, I, GWID(1,I), GWID(2,I),
     *         GWID(3,I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            CALL HIADD (LUN1, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
            IF (ISFREQ) THEN
               IF (APARM(2*I).LE.0.0) THEN
                  HILINE = TSKNAM // '/ independent of channel'
               ELSE
                  WRITE (HILINE,1018) TSKNAM, I, APARM(2*I-1),
     *               APARM(2*I)
                  END IF
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
               CALL HIADD (LUN1, HILINE, BUFF1, IERR)
               IF (IERR.NE.0) GO TO 200
               END IF
 20         CONTINUE
         IF (PCATR(KRBMJ).GT.0.0) THEN
            IF ((BPARM(5).EQ.1.0) .AND. (BPARM(6).EQ.1.0)) THEN
               HILINE = TSKNAM // '/ Convolved Gaussian actually added:'
            ELSE
               HILINE = TSKNAM //
     *            '/ Convolved Gaussian added plus beam:'
               END IF
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            CALL HIADD (LUN1, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
            DO 30 I = 1,NGAUSS
               WRITE (HILINE,1015) TSKNAM, I, DMAX(I,1)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
               CALL HIADD (LUN1, HILINE, BUFF1, IERR)
               IF (IERR.NE.0) GO TO 200
               WRITE (HILINE,1016) TSKNAM, I, GPOS(1,I), GPOS(2,I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
               CALL HIADD (LUN1, HILINE, BUFF1, IERR)
               IF (IERR.NE.0) GO TO 200
               IF ((DFJ(I,1).GT.0.0) .AND. (DFN(I,1).GT.0.0)) THEN
                  XJ = SQRT (4.*LOG(2.) / DFJ(I,1))
                  XN = SQRT (4.*LOG(2.) / DFN(I,1))
                  WRITE (HILINE,1017) TSKNAM, I, XJ, XN, GWID(3,I)
                  CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                  IF (IERR.NE.0) GO TO 200
                  CALL HIADD (LUN1, HILINE, BUFF1, IERR)
                  IF (IERR.NE.0) GO TO 200
                  END IF
 30            CONTINUE
            IF ((BPARM(5).NE.1.0) .OR. (BPARM(6).NE.1.0)) THEN
               HILINE = TSKNAM //
     *           '/ Convolved Gaussian added minus beam:'
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
               CALL HIADD (LUN1, HILINE, BUFF1, IERR)
               IF (IERR.NE.0) GO TO 200
               DO 35 I = 1,NGAUSS
                  WRITE (HILINE,1015) TSKNAM, I, DMAX(I,2)
                  CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                  IF (IERR.NE.0) GO TO 200
                  CALL HIADD (LUN1, HILINE, BUFF1, IERR)
                  IF (IERR.NE.0) GO TO 200
                  WRITE (HILINE,1016) TSKNAM, I, GPOS(1,I), GPOS(2,I)
                  CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                  IF (IERR.NE.0) GO TO 200
                  CALL HIADD (LUN1, HILINE, BUFF1, IERR)
                  IF (IERR.NE.0) GO TO 200
                  IF ((DFJ(I,2).GT.0.0) .AND. (DFN(I,2).GT.0.0)) THEN
                     XJ = SQRT (4.*LOG(2.) / DFJ(I,2))
                     XN = SQRT (4.*LOG(2.) / DFN(I,2))
                     WRITE (HILINE,1017) TSKNAM, I, XJ, XN, GWID(3,I)
                     CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                     IF (IERR.NE.0) GO TO 200
                     CALL HIADD (LUN1, HILINE, BUFF1, IERR)
                     IF (IERR.NE.0) GO TO 200
                     END IF
 35               CONTINUE
               END IF
            END IF
         END IF
C                                       Continuum
      WRITE (HILINE,1030) TSKNAM, BPARM(1)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1031) TSKNAM, BPARM(2)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1032) TSKNAM, BPARM(3)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1033) TSKNAM, APARM(3)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1034) TSKNAM, APARM(6)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1035) TSKNAM, BPARM(5)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1036) TSKNAM, BPARM(6)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (BPARM(4).GT.0.0) THEN
         HILINE = TSKNAM // '/ coordinate corrected before modeling'
      ELSE
         HILINE = TSKNAM // '/ coordinate corrected after modeling'
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Observatory
      WRITE (HILINE,1040) TSKNAM, CPARM(6)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1041) TSKNAM, CPARM(7)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1042) TSKNAM, CPARM(8)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1043) TSKNAM, CPARM(9)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
      CALL HICLOS (LUN1, T, BUFF1, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', FVOL(NCFILE-1), FCNO(NCFILE-1), PCAT, 'REST',
     *   BUFF1, IERR)
      CALL CATIO ('UPDT', FVOL(NCFILE), FCNO(NCFILE), MCAT, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDMOHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1012 FORMAT (A6,'NOISE=',F8.3,5X,'/ Noise added, data discarded')
 1014 FORMAT (A6,'NGAUSS=',I2,10X,'/ Gaussians used')
 1015 FORMAT (A6,'GMAX(',I1,')=',1PE13.5,10X,'/ Peak brightness')
 1016 FORMAT (A6,'GPOS(',I1,')=',F8.1,',',F8.1,6X,
     *   '/ X,Y center in arc seconds')
 1017 FORMAT (A6,'GWID(',I1,')=',F6.1,',',F6.1,',',F6.1,3X,
     *   '/ major,minor arc sec, PA deg')
 1018 FORMAT (A6,'GCHN(',I1,')=',F6.1,',',F5.1,11X,
     *   '/ center,fwhm spectral channels')
 1030 FORMAT (A6,'THROW =',F8.4,5X,'/ formal throw length')
 1031 FORMAT (A6,'SCALE =',F8.4,5X,'/ change throw length')
 1032 FORMAT (A6,'ROTATE=',F8.4,5X,'/ rotate throw ccw (degrees)')
 1033 FORMAT (A6,'XNOISE=',F8.4,5X,'/ noise in az coord')
 1034 FORMAT (A6,'YNOISE=',F8.4,5X,'/ noise in el coord')
 1035 FORMAT (A6,'GRATIO=',F8.4,5X,'/ ratio +/- beam gain')
 1036 FORMAT (A6,'WRATIO=',F8.4,5X,'/ ratio +/- beam width')
 1040 FORMAT (A6,'DIAMETER=',F6.2,5X,'/ telescope diameter (m)')
 1041 FORMAT (A6,'OBS-LONG=',F10.5,1X,'/ observatory long (deg)')
 1042 FORMAT (A6,'OBS-LAT=',F10.5,2X,'/ observatory lat (deg)')
 1043 FORMAT (A6,'OBS-RAD=',F10.1,2X,'/ observatory-Earth center (m)')
      END
      SUBROUTINE BSMOMO (NUMVIS, U, V, T, RESULT, IRET)
C-----------------------------------------------------------------------
C   Computes the plus and minus throw model
C   Inputs:
C      NUMVIS   I      Visibility number, -1 => final call, no data
C                      passed but allows any operations to be completed
C   Inputs from COMMON:
C      RA       D      Right ascension (1950) of phase center. (deg)
C      DEC      D      Declination (1950) of phase center. (deg)
C   In/Output:
C      U        R      Azimuth in degrees
C      V        R      Elevation in degrees
C      T        R      Time in days
C   Output:
C      RESULT   R(2)   Output visibilities selected in frequency.
C      IRET     I      Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      U, V, T, RESULT(4)
C
      INTEGER   I, J, SBUFF(512)
      REAL      X, Y, XJ, XN, Z, RU, RV, ZPOS(2,4), CP, SP, X1, X2
      DOUBLE PRECISION PRA, PDEC, MRA, MDEC, ANTLST, HRANG, PANGL,
     *   SINLAT, COSLAT
      INCLUDE 'BSMOD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      SAVE PRA, PDEC, MRA, MDEC
C-----------------------------------------------------------------------
      IRET = 0
C                                       Init information about
C                                       Gaussians:
      IF (NUMVIS.EQ.1) THEN
         CALL RANDIN (I)
         X1 = PCATR(KRBMJ) * 3600 * SQRT (BPARM(6))
         X2 = PCATR(KRBMJ) * 3600 / SQRT (BPARM(6))
         PRA = -THROW * COS (DG2RAD * BPARM(3)) * BPARM(2) / 3600.
         PDEC= -THROW * SIN (DG2RAD * BPARM(3)) * BPARM(2) / 3600.
         MRA =  THROW * COS (DG2RAD * BPARM(3)) * BPARM(2) / 3600.
         MDEC=  THROW * SIN (DG2RAD * BPARM(3)) * BPARM(2) / 3600.
C                                       Precompute gaussians
         IF (NGAUSS.GT.0) THEN
            DO 10 I = 1,NGAUSS
               IF ((X.LE.0.0) .AND. ((GWID(1,I).LE.0.0) .OR.
     *            (GWID(2,I).LE.0.0))) THEN
                  WRITE (MSGTXT,1000) I
                  CALL MSGWRT (8)
                  IRET = 8
                  GO TO 999
                  END IF
               DFJ(I,1) = 4. * LOG(2.0) / (X1*X1 + GWID(1,I)*GWID(1,I))
               DFN(I,1) = 4. * LOG(2.0) / (X1*X1 + GWID(2,I)*GWID(2,I))
               DFJ(I,2) = 4. * LOG(2.0) / (X2*X2 + GWID(1,I)*GWID(1,I))
               DFN(I,2) = 4. * LOG(2.0) / (X2*X2 + GWID(2,I)*GWID(2,I))
               DMAX(I,1) = GMAX(I) * SQRT (BPARM(5))
               DMAX(I,2) = GMAX(I) / SQRT (BPARM(5))
               IF ((X1.GT.0.0) .AND. (GWID(1,I).GT.0.0) .AND.
     *            (GWID(2,I).GT.0.0)) DMAX(I,1) = DMAX(I,1) * GWID(1,I)
     *            * GWID(2,I) / SQRT ((X1*X1 + GWID(1,I)**2) * (X1*X1 +
     *            GWID(2,I)**2))
               IF ((X2.GT.0.0) .AND. (GWID(1,I).GT.0.0) .AND.
     *            (GWID(2,I).GT.0.0)) DMAX(I,2) = DMAX(I,2) * GWID(1,I)
     *            * GWID(2,I) / SQRT ((X2*X2 + GWID(1,I)**2) * (X2*X2 +
     *            GWID(2,I)**2))
               DDY(I,1) = PDEC + GPOS(2,I)/3600.
               DDC(I,1) = COS (DG2RAD * DDY(I,1))
               DDX(I,1) = PRA * DDC(I,1) + GPOS(1,I) / 3600
               DDY(I,2) = MDEC + GPOS(2,I)/3600.
               DDC(I,2) = COS (DG2RAD * DDY(I,2))
               DDX(I,2) = MRA * DDC(I,2) + GPOS(1,I) / 3600
 10            CONTINUE
            ISFREQ = .FALSE.
            END IF
C                                       Set up antennas etc
         RA = PCATD(KDORA) * DG2RAD
         DEC = PCATD(KDODE) * DG2RAD
         CALL GETANT (FVOL(NCFILE-1), FCNO(NCFILE-1), 1, PCAT, SBUFF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         T = (CPARM(3) * DG2RAD + RA - GSTIAT - STNLON(1)) / ROTIAT
         END IF
C                                       Current record
      IF (NUMVIS.GT.0) THEN
C                                       Antenna LST
         ANTLST = GSTIAT + STNLON(1) + T * ROTIAT
         SINLAT = SIN (STNLAT(1))
         COSLAT = COS (STNLAT(1))
C                                       Hour angle
         HRANG = ANTLST - RA
C                                       Limit to between 0 and 2pi
         HRANG = MOD (HRANG, TWOPI)
C                                       translate to between -pi and pi
         IF (HRANG.GT.PI) HRANG = HRANG - TWOPI
         IF (HRANG.LT.-PI) HRANG = HRANG + TWOPI
C                                       get coordinates
         RU = U
         RV = V
         IF (APARM(3).GT.0.0) THEN
            CALL NOISE (X)
            RU = RU + APARM(3) * X / 3600.0
            END IF
         IF (APARM(6).GT.0.0) THEN
            CALL NOISE (X)
            RV = RV + APARM(6) * X / 3600.0
            END IF
         IF (BPARM(4).GT.0.0) THEN
            U = RU
            V = RV
            END IF
C                                       Parallactic angle
         PANGL = ATAN2 (COSLAT * SIN(HRANG),
     *      (SINLAT * COS(DEC) - COSLAT * SIN(DEC) * COS(HRANG)))
         CP = COS (PANGL)
         SP = SIN (PANGL)
         RESULT(3) = U * CP - V * SP
         RESULT(4) = U * SP + V * CP
C                                       find flux
         DO 120 J = 1,2
            RESULT(J) = 0.
            DO 110 I = 1,NGAUSS
               ZPOS(1,I) = GPOS(1,I) * CP + GPOS(2,I) * SP
               ZPOS(2,I) = -GPOS(1,I) * SP + GPOS(2,I) * CP
               DDY(I,1) = PDEC + ZPOS(2,I)/3600.
               DDC(I,1) = COS (DG2RAD * DDY(I,1))
               DDX(I,1) = PRA * DDC(I,1) + ZPOS(1,I) / 3600
               DDY(I,2) = MDEC + ZPOS(2,I)/3600.
               DDC(I,2) = COS (DG2RAD * DDY(I,2))
               DDX(I,2) = MRA * DDC(I,2) + ZPOS(1,I) / 3600
               X = RU * DDC(I,J) - DDX(I,J)
               Y = RV - DDY(I,J)
               Z = DG2RAD*GWID(3,I)
               XJ = (X * SIN (Z) + Y * COS (Z)) * 3600.
               XN = (Y * SIN (Z) - X * COS (Z)) * 3600.
               Z = DFJ(I,J) * XJ * XJ + DFN(I,J) * XN * XN
               IF (Z.LT.10.) RESULT(J) = RESULT(J) + DMAX(I,J) * EXP(-Z)
 110           CONTINUE
 120        CONTINUE
        IF (FLUX.GT.0) THEN
           CALL NOISE (X)
           RESULT(1) = RESULT(1) + FLUX * X
           CALL NOISE (X)
           RESULT(2) = RESULT(2) + FLUX * X
           END IF
C                                       last call - history
      ELSE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COMPONENT',I2,' MUST HAVE WIDTH OR HEADER BEAM WIDTH')
      END
      SUBROUTINE NOISE (ANOISE)
C-----------------------------------------------------------------------
C   Random noise generator
C    Output: ANOISE  R    Result
C-----------------------------------------------------------------------
      REAL      ANOISE, TEMP
      INTEGER   J
C-----------------------------------------------------------------------
      ANOISE = -6.0
      DO 10 J = 1,12
         CALL RANDUM (TEMP)
         ANOISE = ANOISE + TEMP
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE OTFANT (CCNO, DISKO, UCOR)
C-----------------------------------------------------------------------
C   Make an antenna file for the output data file.
C   Inputs:
C      CCNO    I   Output catalog number
C      DISKO   I      Output disk number
C      UCOR    R(3)   Diameter (m), Longitude, latitude, radius
C-----------------------------------------------------------------------
      INTEGER   CCNO, DISKO
      REAL      UCOR(4)
C
      INTEGER   LUNA,  IABUF(512), FINDA, I, JERR, ANVER, MAXA, J
      DOUBLE PRECISION JDA, GASTM
      LOGICAL   TABLE, EXIST, FITASC
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNA /30/
C-----------------------------------------------------------------------
C                                       Is there one already
      ANVER = 1
      CALL ISTAB ('AN', DISKO, CCNO, ANVER, LUNA, IABUF, TABLE, EXIST,
     *   FITASC, JERR)
C                                       Setup for AN table initization
      NUMORB = 0
      NOPCAL = 2
C                                       Position of the earth's pole
      POLRXY(1) = 0.0
      POLRXY(2) = 0.0
      UT1UTC = 0.0
      DATUTC = 0.0
      TIMSYS = 'IAT'
C                                       Array name
      CALL H2CHR (8, 1, CATH(KHTEL), ANAME)
      J = UCOR(1) + 0.5
C                                       Array center (rel to center of
C                                       earth): shift from VLBA KP one
      IF ((UCOR(4).LE.1.E6) .OR. ((UCOR(2).EQ.0.) .AND.
     *   (UCOR(3).EQ.0.))) THEN
         ARRAYC(1) = -1995945.D0
         ARRAYC(2) = -5037360.D0
         ARRAYC(3) =  3357261.D0
         UCOR(4) = 6374166.13
         UCOR(3) = 31.7826
         UCOR(2) = -111.61487
      ELSE
         ARRAYC(1) = COS (UCOR(3)*DG2RAD) * COS (UCOR(2)*DG2RAD) *
     *      UCOR(4)
         ARRAYC(2) = COS (UCOR(3)*DG2RAD) * SIN (UCOR(2)*DG2RAD) *
     *      UCOR(4)
         ARRAYC(3) = SIN (UCOR(3)*DG2RAD) * UCOR(4)
         END IF
C                                       Get GST0 and Earth rotation rate
      CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
      CALL JULDAY (RDATE, JDA)
      CALL GSTROT (JDA, GSTIA0, GASTM, DEGPDY)
      SAFREQ = CATD(KDCRV+2)
C                                       Create/init file
      NUMORB = 0
      NOPCAL = 2
      ANTNIF = 1
      ANFQID = -1
      CALL ANTINI ('WRIT', IABUF, DISKO, CCNO, ANVER, CATBLK, LUNA,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *   RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME,
     *   NUMORB, NOPCAL, ANTNIF, ANFQID, JERR)
      IF (JERR.NE.0) GO TO 990
      MAXA = IANRNO - 1
      MAXA = MAX (MAXA, 1)
C                                       init basic AN record
      STAXOF = 0.0
      STAXYZ(1) = 0.0D0
      STAXYZ(2) = 0.0D0
      STAXYZ(3) = 0.0D0
      ORBPRM(1) = 0.0D0
      NOSTA = 0
      MNTSTA = 0
      POLAA = 0.0
      POLAB = 0.0
      CALL RFILL (3, 0.0, POLCA)
      CALL RFILL (3, 0.0, POLCB)
      POLTYA = 'R'
      POLTYB = 'L'
      DO 10 I = 1,MAXA
         NOSTA = I
         WRITE (ANNAME,1000) J, I
         IANRNO = I
         CALL TABAN ('WRIT', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA,POLTYB, POLAB, POLCB, JERR)
         IF (JERR.NE.0) GO TO 990
 10      CONTINUE
C                                       Close/update AN file.
      CALL TABIO ('CLOS', 1, IANRNO, IABUF, IABUF, JERR)
      GO TO 999
C                                       Error
 990  CALL ZCLOSE (LUNA, FINDA, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2,'M IF',I2.2)
      END
