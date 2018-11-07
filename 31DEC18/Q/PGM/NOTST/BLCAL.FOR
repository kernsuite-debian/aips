LOCAL INCLUDE 'BLCAL.INC'
C                                                         Include BLCAL
C                                       Local include for BLCAL
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   CATIN(256), SEQIN, SEQ2, DISKIN, DISK2, CNOIN, CNOIN2,
     *   CCTVER, VISDSK, VISCNO, BLVERO, KOLSBL(MAXBLC), NUMVBL(MAXBLC),
     *   LUNBL, JBUFSZ, NCOMP(MAXFLD), IRNOBL, CHNSEL(3,20,MAXIF)
      LOGICAL   SINGLE, DOMODL, BLOPEN
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAME2(3), XCLAS2(2),
     *   XXSOUR(4,30), XMETH, XCMOD
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAS2*6, XSOUR(30)*16,
     *   CMETH*4, CMOD*4
      REAL   XSI, XDI, XBLV, XTIME(8), XBAND, XFREQ, XFQID, XANTS(50),
     *   XSUBA, XFLAG, XDOCAL, XGUSE, XDOPOL, XPDVER, XDOBND, XBPVER,
     *   XSMOTH(3), XS2, XD2, XVER, XNCOMP(MAXAFL), XFLUX, XNMAP,
     *   XSMOD(7), XSOLIN, XCHNS(4,20), BPARM(10), XBADD(10),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS)
      COMMON /CINFO/ NCOMP, IRNOBL, SINGLE, DOMODL, BLOPEN, CATIN,
     *   CNOIN, CNOIN2, CCTVER, VISDSK, VISCNO, BLVERO, KOLSBL, NUMVBL,
     *   LUNBL, DISKIN, DISK2, SEQIN, SEQ2, CHNSEL
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /XINPUT/ XNAMEI, XCLAIN, XSI, XDI, XBLV, XXSOUR, XTIME,
     *   XBAND, XFREQ, XFQID, XANTS, XSUBA, XFLAG, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XDOBND, XBPVER, XSMOTH, XNAME2, XCLAS2, XS2,
     *   XD2, XVER, XNCOMP, XFLUX, XNMAP, XMETH, XCMOD, XSMOD, XSOLIN,
     *   XCHNS, BPARM, XBADD
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAME2, CLAS2, XSOUR, CMETH, CMOD
C                                                          End BLCAL
LOCAL END
      PROGRAM BLCAL
C-----------------------------------------------------------------------
C! Determines correlator based offsets given a model of the source.
C# UV Calibration AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2003-2004, 2006-2010, 2012, 2015-2016
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
C   BLCAL computes baseline dependent correction and writes them into
C   an BL table.  BLCAL divides the specified data by a given model
C   and averages the data for a specified time to determine the
C   corrections.  At present only multiplicative errors are corrected.
C     The object is to read a file containing
C   data from a strong point source (or data divided by model) and
C   averaged over time (eg one visrec from each baseline).  That
C   data will be written in BL table file.
C   Adapted from FUDGE.  Nov. 28 1983  R.C.Walker
C   Averages and statistics added.  18 Apr. 1985   RCW
C   Converted to BLCAL Jan. 1987, W. D. Cotton.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, MAXANS, MAXIFS, NEED, TNEED, IWORK(2)
      LONGINT   BPT, DBPT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BLCAL.INC'
      REAL      WORK(2)
      DOUBLE PRECISION DWORK(2)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      EQUIVALENCE (WORK, DWORK)
      DATA PRGM /'BLCAL '/
C-----------------------------------------------------------------------
C                                       Release AP memory
      CALL QRLSE
C                                       Get input parameters and
C                                       create output file if nec.
      CALL BLCIN (PRGM, MAXANS, MAXIFS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get dynamic memory
      NEED = MAXIFS * MAXANS * MAXANS
      TNEED = (10 * NEED - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, TNEED, WORK, BPT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'FAILED TO ALLOCATED NEEDED MEMORY'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Select data.
      CALL BLCSEL (IRET)
      IF (IRET.NE.0) GO TO 980
C                                       Divide data by model if nec.
      IF (DOMODL) THEN
         CALL BLCMOD (IRET)
         IF (IRET.NE.0) GO TO 980
         END IF
C                                       Average data and write BL table
      DBPT = BPT / 2
      CALL BLCUV (MAXANS, MAXIFS, DWORK(DBPT+1), DWORK(DBPT+1+NEED),
     *   DWORK(DBPT+1+2*NEED), DWORK(DBPT+1+3*NEED), WORK(BPT+1+8*NEED),
     *   IWORK(BPT+1+9*NEED), IRET)
      IF (IRET.NE.0) GO TO 980
C                                       History
      CALL BLCHIS
C                                       free memory
 980  CALL ZMEMRY ('FREE', TSKNAM, TNEED, WORK, BPT, IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE BLCIN (PRGN, MAXANS, MAXIFS, IRET)
C-----------------------------------------------------------------------
C   BLCIN gets input parameters for BLCAL and creates an output file
C   if necessary.
C   Inputs:  PRGN    H(2)      Program name (4 chars/word)
C   Output:  MAXANS  I         Maximum antenna number in data.
C            MAXIFS  I         Maximum number of IFs.
C            IRET    I         Error code: 0 => ok
C                                1 => too few frequency channels.
C                                5 => catalog troubles
C                                7 => Too many ant. for ls.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in BLCAL for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   MAXANS, MAXIFS, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IERR, NPARM, I, MXFLD, NUMSUB, IROUND, LUN1, NUMAN(513),
     *   LUN, NW(MAXIF), J, K, K1, K2
      LOGICAL   T, TABLE, EXIST, FITASC, MATCH
      REAL      CATR(128), CATINR(128)
      CHARACTER STAT*4, UTYPE*2
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'BLCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (NUMAN, BUFF2)
      EQUIVALENCE (CATBLK, CATR, CATH)
      EQUIVALENCE (CATIN, CATINR)
      DATA LUN1/28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS  * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
      NSOUWD = 1
C                                       Get input parameters.
      MXFLD = MAXAFL
      NPARM = 240 + MXFLD + 80
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSI)
      SEQ2 = IROUND (XS2)
      DISKIN = IROUND (XDI)
      DISK2 = IROUND (XD2)
      CCTVER = IROUND (XVER)
      CCTVER = MAX (0, CCTVER)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 10      CONTINUE
C                                       Get CATBLK from old file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 990
         END IF
C                                       Save Input file info
      VISDSK = DISKIN
      VISCNO = CNOIN
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      DO 15 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 15      CONTINUE
C                                       Check sort order, must be T*
      IF (ISORT(1:1).NE.'T') THEN
         IRET = 4
         WRITE (MSGTXT,1070) ISORT, 'T*'
         GO TO 990
         END IF
C                                       Save input header.
      CALL COPY (256, CATBLK, CATIN)
C                                       See if a multiple source file
      LUNS(1) = 29
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNS, BUFF1, TABLE, EXIST,
     *   FITASC, IERR)
      SINGLE = (.NOT.EXIST) .OR. (IERR.NE.0) .OR. (ILOCSU.LT.0)
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 20 I = 1,30
         SOURCS(I) = XSOUR(I)
 20      CONTINUE
      CALL RCOPY (8, XTIME, TIMRNG)
      DXTIME = 0.0
      BCHAN = 1
      ECHAN = CATBLK(KINAX+JLOCF)
      BIF = 1
      EIF = 1
      IF ((JLOCIF.GE.0)) EIF = CATBLK(KINAX+JLOCIF)
C                                       Channel selection
      I = 60 * MAXIF
      CALL FILL (I, 0, CHNSEL)
      CALL FILL (MAXIF, 0, NW)
      DO 40 J = 1,20
         K = IROUND (XCHNS(2,J))
         IF (K.GT.0) THEN
            K = IROUND (XCHNS(4,J))
            IF ((K.LE.0) .OR. (K.GT.MAXIF)) THEN
               K1 = 1
               K2 = MAXIF
            ELSE
               K1 = K
               K2 = K
               END IF
            DO 35 K = K1,K2
               NW(K) = NW(K) + 1
               DO 30 I = 1,3
                  CHNSEL(I,NW(K),K) = IROUND (XCHNS(I,J))
                  IF (CHNSEL(I,NW(K),K).LT.0) CHNSEL(I,NW(K),K) = 0
 30               CONTINUE
               IF (CHNSEL(3,NW(K),K).EQ.0) CHNSEL(3,NW(K),K) = 1
 35            CONTINUE
            END IF
 40      CONTINUE
C                                       If no channel selection
C                                       use VLA definition of
C                                       channel 0
      DO 50 K = 1,MAXIF
         IF (NW(K).LE.0) THEN
            NW(K) = 1
            CHNSEL(1,1,K) = (ECHAN+1)/8 + 1
            CHNSEL(2,1,K) = ECHAN - ((ECHAN+1)/8)
            CHNSEL(3,1,K) = 1
            END IF
         DO 45 I = 1,NW(K)
            CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), ECHAN))
            IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K))
     *         CHNSEL(2,I,K) = ECHAN
            CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), ECHAN))
 45         CONTINUE
 50      CONTINUE
C                                       Antennas
      DO 85 I = 1,50
         ANTENS(I) = IROUND (XANTS(I))
 85      CONTINUE
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Smoothing type
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      FGVER = IROUND (XFLAG)
      CLVER = 0
      CLUSE = IROUND (XGUSE)
      BLVERO = IROUND (XBLV)
      BLVER = -1
C                                       See if do divide by model
      DOMODL = BPARM(1) .LE. 0.5
C                                       Default averaging time
      XSOLIN = XSOLIN / (24.0 * 60.0)
      IF (XSOLIN.LE.1.0E-20) XSOLIN = 1.0E10
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
      NUMSUB = MAX (1, NUMSUB)
      IF (NUMSUB.LT.SUBARR) THEN
         WRITE (MSGTXT,1160) SUBARR, NUMSUB
         GO TO 990
         END IF
C                                       Find number of antennas.
      CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN1, BUFF1, NUMAN, IRET)
      IF (IRET.NE.0) GO TO 999
      NUMANT = NUMAN(2)
      IF (SUBARR.LE.NUMAN(1)) NUMANT = NUMAN(1+SUBARR)
      MAXANS = NUMANT
C                                       Number of IFs
      NUMIF = 1
      IF (JLOCIF.GE.0) NUMIF = CATBLK(KINAX+JLOCIF)
      MAXIFS = NUMIF
C                                       Number of polarizations
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I4)
 1050 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('SORT ORDER IS ',A2,' NOT ',A2,' AS REQUIRED')
 1160 FORMAT ('SPECIFIED SUBARRAY ',I4,' > MAX. OF ',I4)
      END
      SUBROUTINE BLCUV (MAXANS, MAXIFS, SUMP1, SUMP2, SUM2P1, SUM2P2,
     *   SWT, COUNT, IRET)
C-----------------------------------------------------------------------
C   BLCUV averages data and writes the BL table.
C   Input:
C      MAXANS   I      Number of antennas (array dimension)
C      MAXIFS   I      Number of IFs (array dimension)
C   Output:
C      SUMP1    D(*)   Work array. Sums for polarization 1
C                      (IF,ant1,ant2) two halves for real and imag.
C      SUMP2    D(*)   Work array. Sums for polarization 2
C      SUM2P1   D(*)   Work array. Sums**2 for polarization 1
C      SUM2P1   D(*)   Work array. Sums**2 for polarization 2
C      COUNT    I(*)   Work array. counts
C      IRET     I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   MAXANS, MAXIFS, IRET
      DOUBLE PRECISION SUMP1(MAXIFS,MAXANS,MAXANS),
     *   SUMP2(MAXIFS,MAXANS,MAXANS),
     *   SUM2P1(MAXIFS,MAXANS,MAXANS), SUM2P2(MAXIFS,MAXANS,MAXANS)
      REAL      SWT(MAXIFS,MAXANS,MAXANS)
      INTEGER   COUNT(MAXIFS,MAXANS,MAXANS)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TYPE*2, IFILE*48
      INTEGER   INIO, IPTRI, LUNI, NFREQ, INDEX, IOFF, INDI, IDSOU,
     *   ILENBU, IBIND, I, IA1, IA2, LPIF, LPFRQ, VDSK, VCNO, BO, VO,
     *   NUMVIS, XCOUNT, NUGOOD, II
      REAL      TMSRT, TIMEND, VISRE, VISIM, WT, CHFLGS(MAXCIF)
      LOGICAL   T, F
      INCLUDE 'BLCAL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI /16/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      BLOPEN = F
      CALL CHWANT (ECHAN, MAXIFS, CHNSEL, CHFLGS)
C                                       Initialize arrays.
      NUGOOD = 0
      DO 30 IA2 = 1,MAXANS
         DO 20 IA1 = 1,MAXANS
            DO 10 I = 1,MAXIFS
               SUMP1(I,IA1,IA2) = 0.0D0
               SUMP2(I,IA1,IA2) = 0.0D0
               SUM2P1(I,IA1,IA2) = 0.0D0
               SUM2P2(I,IA1,IA2) = 0.0D0
               SWT(I,IA1,IA2) = 0.0
               COUNT(I,IA1,IA2) = 0
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
      NFREQ = CATBLK(KINAX+JLOCF)
C                                       Init. Source ID number
      IDSOU = 0
      IF (.NOT.SINGLE) IDSOU = SOUWAN(1)
C                                       Open and init for read
C                                       visibility file
      TYPE = 'UV'
      VDSK = VISDSK
      VCNO = VISCNO
      IF (VISDSK.LE.0) THEN
         VDSK = SCRVOL(VISCNO)
         VCNO = SCRCNO(VISCNO)
         TYPE = 'SC'
         END IF
      CALL ZPHFIL (TYPE, VDSK, VCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, VDSK, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 999
         END IF
C                                       Init vis file for write
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
 100     CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
C                                       Set initial time range.
         IF (NUMVIS.EQ.0) TMSRT = BUFF1(IPTRI+ILOCT)
         IF (NUMVIS.EQ.0) TIMEND = BUFF1(IPTRI+ILOCT) + XSOLIN
         DO 190 I = 1,INIO
            IF (ILOCB.GE.0) THEN
               IA1 = BUFF1(IPTRI+ILOCB) / 256. + 0.1
               IA2 = BUFF1(IPTRI+ILOCB) - IA1*256. + 0.1
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
            IF ((.NOT.SINGLE) .AND. (NSOUWD.GT.1)) IDSOU =
     *         BUFF1(IPTRI+ILOCSU) + 0.1
            NUMVIS = NUMVIS + 1
C                                       Check if through with average.
            IF ((BUFF1(IPTRI+ILOCT) .GT. TIMEND) .AND. (NUGOOD.GT.0))
     *         THEN
               CALL BLCOUT (IDSOU, TMSRT, TIMEND, MAXANS, MAXIFS,
     *            SUMP1, SUMP2, SUM2P1, SUM2P2, SWT, COUNT, IRET)
               IF (IRET.NE.0) GO TO 999
               NUGOOD = 0
C                                       Reset times if necessary
               IF (BUFF1(IPTRI+ILOCT) .GT. TIMEND) THEN
                  TMSRT = BUFF1(IPTRI+ILOCT)
                  TIMEND = TMSRT + XSOLIN
                  END IF
               END IF
C                                       Sum data
            NUGOOD = NUGOOD + 1
C                                       Loop over IF
            II = 0
            DO 140 LPIF = 1,NUMIF
               IOFF = IPTRI + NRPARM + (LPIF-1) * INCIF
C                                       Sum frequencies
               DO 130 LPFRQ = 1,NFREQ
                  II = II + 1
C                                       Get data pointer 1st poln.
                  INDEX = IOFF + (LPFRQ-1) * INCF
                  VISRE = BUFF1(INDEX)
                  VISIM = BUFF1(INDEX+1)
C                                       Sum first polarization.
                  WT = BUFF1(INDEX+2) * CHFLGS(II)
                  IF (WT.GT.1.0E-20) THEN
                     SUMP1(LPIF,IA1,IA2) = SUMP1(LPIF,IA1,IA2) +
     *                  VISRE * WT
                     SUMP1(LPIF,IA2,IA1) = SUMP1(LPIF,IA2,IA1) +
     *                  VISIM * WT
                     SUM2P1(LPIF,IA1,IA2) = SUM2P1(LPIF,IA1,IA2) +
     *                  VISRE * VISRE * WT
                     SUM2P1(LPIF,IA2,IA1) = SUM2P1(LPIF,IA2,IA1) +
     *                  VISIM * VISIM * WT
                     SWT(LPIF,IA1,IA2) = SWT(LPIF,IA1,IA2) + WT
                     COUNT(LPIF,IA1,IA2) = COUNT(LPIF,IA1,IA2) + 1
                     END IF
C                                       Get data pointer 2nd poln.
                  INDEX = INDEX + INCS
                  VISRE = BUFF1(INDEX)
                  VISIM = BUFF1(INDEX+1)
                  WT = BUFF1(INDEX+2) * CHFLGS(II)
C                                       Sum second polarization.
                  IF ((WT.GT.1.0E-20) .AND. (NUMPOL.GT.1)) THEN
                     SUMP2(LPIF,IA1,IA2) = SUMP2(LPIF,IA1,IA2) +
     *                  VISRE * WT
                     SUMP2(LPIF,IA2,IA1) = SUMP2(LPIF,IA2,IA1) +
     *                  VISIM * WT
                     SUM2P2(LPIF,IA1,IA2) = SUM2P2(LPIF,IA1,IA2) +
     *                  VISRE * VISRE * WT
                     SUM2P2(LPIF,IA2,IA1) = SUM2P2(LPIF,IA2,IA1) +
     *                  VISIM * VISIM * WT
                     SWT(LPIF,IA2,IA1) = SWT(LPIF,IA2,IA1) + WT
                     COUNT(LPIF,IA2,IA1) = COUNT(LPIF,IA2,IA1) + 1
                     END IF
 130              CONTINUE
 140           CONTINUE
C                                       Increment buffer pointer
            IPTRI = IPTRI + LREC
 190     CONTINUE
      GO TO 100
C                                       Final call to BLCOUT
 200  IF (NUGOOD.GT.0) CALL BLCOUT (IDSOU, TMSRT, TIMEND, MAXANS,
     *   MAXIFS, SUMP1, SUMP2, SUM2P1, SUM2P2, SWT, COUNT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL TABIO ('CLOS', 1, IRNOBL, BUFF1, BLBUFF, IRET)
      IF (IRET.LE.0) GO TO 999
         WRITE (MSGTXT,1200) IRET
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLCUV: ERROR',I3,' OPEN VIS FILE')
 1030 FORMAT ('BLCUV: ERROR',I3,' INIT VIS FILE')
 1100 FORMAT ('BLCUV: ERROR',I3,' READING VIS FILE')
 1200 FORMAT ('BLCUV: ERROR',I3,' CLOSING BL TABLE')
      END
      SUBROUTINE BLCOUT (IDSOU, TMSRT, TIMEND, MAXANS, MAXIFS, SUMP1,
     *   SUMP2, SUM2P1, SUM2P2, SWT, COUNT, IRET)
C-----------------------------------------------------------------------
C   BLCOUT computes averages, writes BL table and gives RMSs for the
C   baselines in the message file.
C   Inputs:
C      IDSOU    I      Source ID number
C      MAXANS   I      Number of antennas (array dimension)
C      MAXIFS   I      Number of IFs (array dimension)
C   Input/Output:
C      TMSRT    R      Start time in days, reset to TIMEND on return
C      TIMEND   R      End time in days, reset on return
C      SUMP1    D()    Sum of visibilities 1st poln., (IF, ant1, ant2),
C                      Real part in ant1<ant2 and Imag part in
C                      ant2<ant1.   Zeroed on return.
C      SUMP2    D()    Sum of visibilities 2nd poln., zeroed on return.
C      SUM2P1   D()    Sum of sq. of vis. 1st poln., zeroed on return.
C      SUM2P2   D()    Sum of sq. of vis. 2nd poln., zeroed on return.
C      SWT      R()    Sum WT, (IF, Ant1, Ant2) Poln 1 in Ant1<Ant2
C      COUNT    I()    Counts, (IF, Ant1, Ant2) Poln 1 in Ant1<Ant2
C                      Poln. 2 in Ant2<Ant1. zeroed on return.
C   Inputs from COMMON:
C      BLVERO   I      BL table version number
C      DISKIN   I      Input file disk number
C      CNOIN    I      Input file catalog slot number
C      CATIN    I()    Input file catalog header
C      LUNBL    I      LUN for BL table
C      NUMANT   I      Number of antennas
C      NUMPOL   I      Number of polarizations
C      NUMIF    I      Number of IFs
C      XSOLIN   R      Solution time in days.
C      SUBARR   I      Subarray number
C   Input/Output in common:
C      BLOPEN   L      True if BL table open.
C      IRNOBL   I      BL table number of next entry.
C     NUMVBL(MAXBLC) I    Column numbers for TABBL
C     KOLSBL(MAXBLC) I    Column pointers for TABBL
C     BLBUFF(*)  I    Buffer used for I/O the BL table.
C   Output:
C      IRET       I    Return code   0 => OK
C                                   >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   MAXANS, MAXIFS, IDSOU, IRET
      REAL      SWT(MAXIFS,MAXANS,MAXANS)
      INTEGER   COUNT(MAXIFS,MAXANS,MAXANS)
      REAL      TMSRT, TIMEND, TIME
      DOUBLE PRECISION SUMP1(MAXIFS,MAXANS,MAXANS),
     *   SUMP2(MAXIFS,MAXANS,MAXANS),
     *   SUM2P1(MAXIFS,MAXANS,MAXANS), SUM2P2(MAXIFS,MAXANS,MAXANS)
      INTEGER   IANT1, IANT2, LOOPIF, LIMIT1, LIMIT2, ITR(6)
      REAL      AMP1, AMP2, PHP1, PHP2, RMSAP1, RMSAP2, RMSPP1, RMSPP2,
     *   RP1, RP2, IP1, IP2
      DOUBLE PRECISION XDIV
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NZERO
      PARAMETER (NZERO=4*MAXIF)
      REAL      FACMUL(2,2,MAXIF), FACADD(2,2,MAXIF), TEMP
      INCLUDE 'BLCAL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA FACADD /NZERO*0.0/
C-----------------------------------------------------------------------
      CALL RFILL (NZERO, FBLANK, FACMUL)
C                                       Initialize BL Table
      IF (.NOT.BLOPEN) THEN
         LUNBL = 29
         CALL BLINI ('WRIT', BLBUFF, DISKIN, CNOIN, BLVERO, CATIN,
     *      LUNBL, IRNOBL, KOLSBL, NUMVBL, NUMANT, NUMPOL, NUMIF, IRET)
         IF (IRET.GT.0) GO TO 999
         BLOPEN = .TRUE.
C                                       Mark in time order iff it is
C                                       now empty
         BLBUFF(43) = 0
         IF (IRNOBL.LE.1) BLBUFF(43) = KOLSBL(1)
C                                       Warn user if solutions will
C                                       be appended to an existing
C                                       BL table.
         IF (IRNOBL.GT.1) THEN
            MSGTXT = 'WARNING - Solutions appended to an existing' //
     *         ' BL table'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Average data and write BL Table
      TIME = (TMSRT + TIMEND) * 0.5
      IF (TIME.GT.1.0E3) TIME = TMSRT
      LIMIT1 = NUMANT - 1
      DO 200 IANT1 = 1,LIMIT1
         LIMIT2 = IANT1 + 1
         DO 190 IANT2 = LIMIT2,NUMANT
C                                       Loop over IF
            DO 120 LOOPIF = 1,NUMIF
C                                       Blank if bad
               FACMUL(1,1,LOOPIF) = FBLANK
               FACMUL(2,1,LOOPIF) = FBLANK
               FACMUL(1,2,LOOPIF) = FBLANK
               FACMUL(2,2,LOOPIF) = FBLANK
C                                       Average 1st poln
               IF (SWT(LOOPIF,IANT1,IANT2).GT.0.0) THEN
                  XDIV = 1.0D0 / SWT(LOOPIF,IANT1,IANT2)
                  SUMP1(LOOPIF,IANT1,IANT2) = SUMP1(LOOPIF,IANT1,IANT2)
     *               * XDIV
                  SUMP1(LOOPIF,IANT2,IANT1) = SUMP1(LOOPIF,IANT2,IANT1)
     *               * XDIV
                  SUM2P1(LOOPIF,IANT1,IANT2) =
     *               SUM2P1(LOOPIF,IANT1,IANT2) * XDIV
                  SUM2P1(LOOPIF,IANT2,IANT1) =
     *               SUM2P1(LOOPIF,IANT2,IANT1) * XDIV
C                                       Convert to corrections
                  RP1 = SUMP1(LOOPIF,IANT1,IANT2)
                  IP1 = SUMP1(LOOPIF,IANT2,IANT1)
                  AMP1 = RP1 * RP1 + IP1* IP1
                  IF (AMP1.LE.1.0E-15) AMP1 = 1.0E-15
                  FACMUL(1,1,LOOPIF) = RP1 / AMP1
                  FACMUL(2,1,LOOPIF) = -IP1 / AMP1
                  END IF
C                                       Compute RMS
               IF (COUNT(LOOPIF,IANT1,IANT2).GT.2) THEN
                  XDIV = 1.0D0 / (COUNT(LOOPIF,IANT1,IANT2) - 1)
                  SUM2P1(LOOPIF,IANT1,IANT2) =
     *               (SUM2P1(LOOPIF,IANT1,IANT2) -
     *               SUMP1(LOOPIF,IANT1,IANT2) *
     *               SUMP1(LOOPIF,IANT1,IANT2)) * XDIV
                  SUM2P1(LOOPIF,IANT2,IANT1) =
     *               (SUM2P1(LOOPIF,IANT2,IANT1) -
     *               SUMP1(LOOPIF,IANT2,IANT1) *
     *               SUMP1(LOOPIF,IANT2,IANT1)) * XDIV
                  END IF
C                                       Average 2nd poln
               IF (SWT(LOOPIF,IANT2,IANT1).GT.0) THEN
                  XDIV = 1.0D0 / SWT(LOOPIF,IANT2,IANT1)
                  SUMP2(LOOPIF,IANT1,IANT2) = SUMP2(LOOPIF,IANT1,IANT2)
     *               * XDIV
                  SUMP2(LOOPIF,IANT2,IANT1) = SUMP2(LOOPIF,IANT2,IANT1)
     *               * XDIV
                  SUM2P2(LOOPIF,IANT1,IANT2) =
     *               SUM2P2(LOOPIF,IANT1,IANT2) * XDIV
                  SUM2P2(LOOPIF,IANT2,IANT1) =
     *               SUM2P2(LOOPIF,IANT2,IANT1) * XDIV
C                                       Convert to corrections
                  RP2 = SUMP2(LOOPIF,IANT1,IANT2)
                  IP2 = SUMP2(LOOPIF,IANT2,IANT1)
                  AMP2 = RP2 * RP2 + IP2* IP2
                  IF (AMP2.LE.1.0E-15) AMP2 = 1.0E-15
                  FACMUL(1,2,LOOPIF) = RP2 / AMP2
                  FACMUL(2,2,LOOPIF) = -IP2 / AMP2
                  END IF
C                                       Compute RMS
               IF (COUNT(LOOPIF,IANT2,IANT1).GT.2) THEN
                  XDIV = 1.0D0 / (COUNT(LOOPIF,IANT2,IANT1)-1)
                  SUM2P2(LOOPIF,IANT1,IANT2) =
     *               (SUM2P2(LOOPIF,IANT1,IANT2) -
     *               SUMP2(LOOPIF,IANT1,IANT2) *
     *               SUMP2(LOOPIF,IANT1,IANT2)) * XDIV
                  SUM2P2(LOOPIF,IANT2,IANT1) =
     *               (SUM2P2(LOOPIF,IANT2,IANT1) -
     *               SUMP2(LOOPIF,IANT2,IANT1) *
     *               SUMP2(LOOPIF,IANT2,IANT1)) * XDIV
                  END IF
 120           CONTINUE
C                                       Write BL table entry
            CALL TABBL ('WRIT', BLBUFF, IRNOBL, KOLSBL, NUMVBL,
     *         NUMPOL, TIME, IDSOU, SUBARR, IANT1, IANT2, FRQSEL,
     *         FACMUL, FACADD, IRET)
            IF (IRET.NE.0) GO TO 999
 190        CONTINUE
 200     CONTINUE
C                                       Print statistics
      IF (BPARM(2).GT.0.0) THEN
C                                       Timerange
         TEMP = TMSRT
         ITR(1) = TEMP
         TEMP = (TEMP - ITR(1)) * 24.0
         ITR(2) = TEMP
         TEMP = (TEMP - ITR(2)) * 60.0
         ITR(3) = TEMP + 0.5
         TEMP = 0.0
         IF (TIMEND.LT.1.0E3) TEMP = TIMEND
         ITR(4) = TEMP
         TEMP = (TEMP - ITR(4)) * 24.0
         ITR(5) = TEMP
         TEMP = (TEMP - ITR(5)) * 60.0
         ITR(6) = TEMP + 0.5
C                                       Label and timerange
         IF (TIMEND.LT.1.0E3) THEN
            WRITE (MSGTXT,1200) ITR
         ELSE
            MSGTXT = 'Correlator statistics - all data averaged'
            END IF
         CALL MSGWRT (6)
C                                       Key
         MSGTXT = 'Values given are in the order:'
         CALL MSGWRT (5)
         MSGTXT = 'Amp pol1(RMS), Phase(RMS) Amp pol2(RMS) Phase(RMS)'
         CALL MSGWRT (5)
C                                       Do 1 IF per pass
         DO 500 LOOPIF = 1,NUMIF
            LIMIT1 = NUMANT - 1
            DO 490 IANT1 = 1,LIMIT1
               LIMIT2 = IANT1 + 1
               DO 480 IANT2 = LIMIT2,NUMANT
                  IF ((COUNT(LOOPIF,IANT1,IANT2).GT.0) .OR.
     *               (COUNT(LOOPIF,IANT2,IANT1).GT.0)) THEN
C                                       Convert to amp, phase
                     RP1 = SUMP1(LOOPIF,IANT1,IANT2)
                     RP2 = SUMP2(LOOPIF,IANT1,IANT2)
                     IP1 = SUMP1(LOOPIF,IANT2,IANT1)
                     IP2 = SUMP2(LOOPIF,IANT2,IANT1)
                     AMP1 = SQRT (RP1*RP1 + IP1*IP1)
                     IF (AMP1.LT.1.0E-15) AMP1 = 1.0E-15
                     AMP2 = SQRT (RP2*RP2 + IP2*IP2)
                     IF (AMP2.LT.1.0E-15) AMP2 = 1.0E-15
                     PHP1 = ATAN2 (IP1, RP1+1.0E-20)
                     PHP2 = ATAN2 (IP2, RP2+1.0E-20)
C                                       Convert RMS to amp, phase
                     RMSAP1 = SQRT ((SUM2P1(LOOPIF,IANT1,IANT2)*RP1*RP1)
     *                 + (SUM2P1(LOOPIF,IANT2,IANT1)*IP1*IP1)) * 2.0
     *                 / AMP1
                     RMSAP2 = SQRT ((SUM2P2(LOOPIF,IANT1,IANT2)*RP2*RP2)
     *                 + (SUM2P2(LOOPIF,IANT2,IANT1)*IP2*IP2)) * 2.0
     *                 / AMP2
                     RMSPP1 = SQRT ((SUM2P1(LOOPIF,IANT1,IANT2) *
     *                  ((-IP1 / (AMP1*AMP1))**2)) +
     *                  (SUM2P1(LOOPIF,IANT2,IANT1) *
     *                  ((RP1 / (AMP1*AMP1))**2)))
                     RMSPP2 = SQRT ((SUM2P2(LOOPIF,IANT1,IANT2) *
     *                  ((-IP2 / (AMP2*AMP2))**2)) +
     *                  (SUM2P2(LOOPIF,IANT2,IANT1) *
     *                  ((RP2 / (AMP2*AMP2))**2)))
C                                       Phases to degrees
                     PHP1 = PHP1 * 57.296
                     PHP2 = PHP2 * 57.296
                     RMSPP1 = RMSPP1 * 57.296
                     RMSPP2 = RMSPP2 * 57.296
                     IF (RMSPP1.GT.99.9) RMSPP1 = 99.9
                     IF (RMSPP2.GT.99.9) RMSPP2 = 99.9
                     IF (RMSAP1.GT.9.999) RMSAP1 = 9.99
                     IF (RMSAP2.GT.9.999) RMSAP2 = 9.99
                     WRITE (MSGTXT,1210) IANT1, IANT2, LOOPIF, AMP1,
     *                  RMSAP1, PHP1, RMSPP1, AMP2, RMSAP2, PHP2, RMSPP2
                     CALL MSGWRT (5)
                     END IF
 480              CONTINUE
 490           CONTINUE
 500        CONTINUE
         END IF
C                                       Zero arrays
      DO 730 IANT2 = 1,NUMANT
         DO 720 IANT1 = 1,NUMANT
            DO 710 LOOPIF = 1,NUMIF
               SUMP1(LOOPIF,IANT1,IANT2) = 0.0D0
               SUMP2(LOOPIF,IANT1,IANT2) = 0.0D0
               SUM2P1(LOOPIF,IANT1,IANT2) = 0.0D0
               SUM2P2(LOOPIF,IANT1,IANT2) = 0.0D0
               SWT(LOOPIF,IANT1,IANT2) = 0.0
               COUNT(LOOPIF,IANT1,IANT2) = 0
 710           CONTINUE
 720        CONTINUE
 730     CONTINUE
C                                       Reset times
      TMSRT = TIMEND
      TIMEND = TMSRT + XSOLIN
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('Correlator statistics timerange ',I3,'/',2I3,':',I3,'/',
     *   2I3)
 1210 FORMAT (I2,'-',I2,' IF=',I2,2(F7.4,'(',F6.4,')',F8.2,'(',F5.2,
     *   ')'))
      END
      SUBROUTINE BLCSEL (IRET)
C-----------------------------------------------------------------------
C   BLCSEL will read a multi source data set into a temporary scratch
C   file.  Editing and calibration may be applied.
C   Inputs via common /SELCAL/  (Include DSEL.INC)
C      UNAME(3)     R    AIPS name of input file.
C      UCLAS(2)     R    AIPS class of input file.
C      UDISK        R    AIPS disk of input file.
C      USEQ         R    AIPS sequence of input file.
C      SOURCS(4,30) R    Names (16 char) of up to 30 sources, *=>all
C                        First character of name '-' => all except those
C                        specified.
C      TIMRNG(8)    R    Start day, hour, min, sec, end day, hour,
C                        min,sec. 0's => all
C      UVRNG(2)     R    Minimum and maximum baseline lengths in
C                        1000's wavelengths. 0's => all
C      ANTENS(50)   I    List of antennas selected, 0=>all,
C                        any negative => all except those specified
C      FGVER        I    FLAG file version number, if .le. 0 then
C                        NO flagging is applied.
C      CLUSE        I    Cal file version number to apply.
C   Output:
C      IRET         I    Error code: 0 => OK,
C                        -1 => end of data
C                        >0 => failed, abort process.
C-----------------------------------------------------------------------
      INTEGER   IRET, LUN1, LUN2, IIVER, OOVER
      REAL     DUM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'BLCAL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      REAL WORKR(MAXIF)
      DATA LUN1, LUN2 /28, 29/
C-----------------------------------------------------------------------
C                                       Setup
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.GT.0) GO TO 999
C                                       No data
      IF ((NVIS.LE.0) .OR. (IRET.LT.0)) THEN
         CALL UVGET ('CLOS', DUM, DUM, IRET)
         IRET = -1
C                                       Message
      ELSE
         IF (DOFLAG) THEN
            MSGTXT = 'Selecting, editing and calibrating the data'
         ELSE
            MSGTXT = 'Selecting and calibrating the data'
            END IF
         CALL MSGWRT (4)
C                                       Copy
         VISDSK = 0
         VISCNO = 0
         CALL CALCOP (VISDSK, VISCNO, BUFF1, JBUFSZ, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy relevant portion of IF
C                                       table.
         IIVER = 1
         OOVER = 1
         CALL CHNCOP (IIVER,OOVER, LUN1, LUN2, DISKIN, SCRVOL(VISCNO),
     *      CNOIN, SCRCNO(VISCNO), CATUV, CATBLK, BIF, EIF, FRQSEL,
     *      SFREQS, BUFF1, BUFF2, UBUFF, WORKR, IRET)
            GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE BLCMOD (IRET)
C-----------------------------------------------------------------------
C   BLCMOD divides the CLEAN model visibilities into the data.
C   If no model is found or a point model is specified then the data
C   is divided by the flux density found in the Source (SU) table.
C   Inputs: from commons
C     XNIT      R    Number of components to be divided.
C     DISKIN    R    Input file disk number.
C     CNOIN     I    Input file catalog number.
C     DISK2     R    CLEAN file disk number.
C     XNMAP     R    Number of model files.
C     CCTVER    I    CC table version number.
C     BPARM(1)  R    If .lt. 0 use no model, if .gt. 0 use model
C     FRQSEL    I    FQ ID number
C   Output:
C     CNOIN2    I    CLEAN file catalog number.
C     IRET      I    Return code, 0 => ok, otherwise not.
C-----------------------------------------------------------------------
      INTEGER   IRET
      INTEGER   MODEL, METHOD, ISTOKE, DISKO, ISCR, CHAN, NCHAN, I, IIF,
     *   IROUND
      LOGICAL   DOMSG, F, NONAM, NOCLAS, WASOME
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INTEGER   BITER(MAXFLD)
      INCLUDE 'BLCAL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSCD.INC'
C
      REAL RBUF(MAXIF)
C
      DATA DOMSG, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if multiple sources
      IF (SINGLE) NSOUWD = 1
      IF (NSOUWD.NE.1) GO TO 300
C                                       If neither a point (XSMOD) nor
C                                       clean model use source table.
      NONAM = NAME2 .EQ. '            '
      NOCLAS = CLAS2 .EQ. '      '
      IF (NONAM .AND. NOCLAS .AND. (XSMOD(1).LE.1.0E-20)) GO TO 300
C                                       Set model and method
      CALL H2CHR (4, 1, XMETH, CMOD)
      MODEL = 0
      IF (CMOD.EQ.'COMP') MODEL = 1
      IF (CMOD.EQ.'IMAG') MODEL = 2
      IF (CMOD.EQ.'SUBI') MODEL = 3
      CALL H2CHR (4, 1, XMETH, CMETH)
      METHOD = 0
      IF (CMETH.EQ.'DFT') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
C                                       Point source parameters
      DOPTMD = ABS (XSMOD(1)) .GT. 1.0E-20
      PTFLX = XSMOD(1)
      PTRAOF = XSMOD(2)
      PTDCOF = XSMOD(3)
C                                       Point model only
      PARMOD(1) = 0.0
C                                       Get info on model file(s)
      LIMFLX = XFLUX
      MFIELD = IROUND (XNMAP)
      IF (MFIELD.LE.0) MFIELD = 1
      NONEG = F
      WASOME = F
      DO 10 I = 1,MFIELD
         BITER(I) = 1
         IF (I.LE.MAXAFL) THEN
            NCOMP(I) = ABS (XNCOMP(I)) + 0.1
            IF (XNCOMP(I).LE.-0.5) NONEG = .TRUE.
            IF (NCOMP(I).GT.0) WASOME = .TRUE.
         ELSE
            NCOMP(I) = 0
            IF (WASOME) NCOMP(I) = 1000000000
            END IF
 10      CONTINUE
C                                       Set factor to multiply
      FACGRD(1) = 1.0
      FACGRD(2) = 1.0
      IF (DOPTMD) THEN
         DO3DIM = .FALSE.
      ELSE
         CALL SETGDS (DISKIN, CNOIN, NAME2, CLAS2, SEQ2, DISK2, MFIELD,
     *      CCTVER, NCOMP, BITER, MODEL, METHOD, BUFF1, BUFF2, ISTOKE,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IF (MODEL.GT.0) THEN
            IF (MODEL.EQ.3) THEN
               MSGTXT = 'Using sub-images for the source model'
            ELSE IF (MODEL.EQ.2) THEN
               MSGTXT = 'Using images for the source model'
            ELSE
               MSGTXT = 'Using Clean Component source model'
               END IF
            CALL MSGWRT (3)
            CALL FACSET (DISKIN, CNOIN, 1, SOUWAN(1), MODEL, 1.0, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
      CNOIN2 = CCCNO(1)
C                                       Divide data by model
      DISKO = VISDSK
      ISCR = VISCNO
      CHAN = 1
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      COMPDT = .FALSE.
      DATDIV = .TRUE.
C                                       Consider whether to process
C                                       1 IF at a time
      IF ((NUMIF.GT.1) .AND. (MODEL.EQ.1) .AND.
     *   (ABS(FACGRD(1)-1.0).GT.1.E-10)) THEN
C                                       number of channels
         NCHAN = CATBLK(KINAX+JLOCF)
C                                       For each IF
         DO 15 IIF = 1,NUMIF
C                                       Already know IF 1 scale
            IF (IIF.GT.1) THEN
C                                       Reset Components for div
               IF (MFIELD.GT.0) THEN
                  DO 12 I = 1,MFIELD
                     BITER(I) = 1
                     IF (I.LE.MAXAFL) THEN
                        NCOMP(I) = ABS (XNCOMP(I)) + 0.1
                     ELSE
                        NCOMP(I) = 0
                        IF (WASOME) NCOMP(I) = 1000000000
                        END IF
 12                  CONTINUE
                  CALL SETGDS (DISKIN, CNOIN, NAME2, CLAS2, SEQ2, DISK2,
     *               MFIELD, CCTVER, NCOMP, BITER, MODEL, METHOD, BUFF1,
     *               BUFF2, ISTOKE, IRET)
                  IF (IRET.NE.0) GO TO 999
                  XVER = CCTVER
                  CNOIN2 = CCCNO(1)
                  END IF
C                                       Divide data by model
               DISKO = VISDSK
               ISCR = VISCNO
C                                       Set division parameters
               COMPDT = .FALSE.
               DATDIV = .TRUE.
               FACGRD(1) = 1.0
               IF (MODEL.GT.0) THEN
                  CALL FACSET (DISKIN, CNOIN, IIF, SOUWAN(1), MODEL,
     *               1.0, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
               END IF
C                                       start channel
            CHAN = 1 + (NCHAN * (IIF-1))
C                                       Divide 1 IF by model
            CALL UVMDIV (VISDSK, VISCNO, DISKO, ISCR, MODEL, METHOD,
     *         DOMSG, CHAN, NCHAN, CATBLK, JBUFSZ, FRQSEL, BUFF1,
     *         BUFF2, UBUFF, RBUF, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL UNSETG (BUFF2)
            DOMSG  = .FALSE.
 15         CONTINUE
      ELSE
         CHAN = 1
         NCHAN = CATBLK(KINAX+JLOCF) * NUMIF
C                                       CC components to unity
         CALL UVMDIV (VISDSK, VISCNO, DISKO, ISCR, MODEL, METHOD,
     *      DOMSG, CHAN, NCHAN, CATBLK, JBUFSZ, FRQSEL, BUFF1, BUFF2,
     *      UBUFF, RBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (.NOT.DOPTMD) CALL UNSETG (BUFF2)
         END IF
C                                       Get true values of NCOMP
      DO 20 I = 1,MFIELD
         NCOMP(I) = NSUBG(I) - 1
 20      CONTINUE
C                                       Model divided by data now
C                                       in scratch file.
      VISDSK = 0
      VISCNO = ISCR
      GO TO 999
C                                       Multiple sources, use point
C                                       source at phase center only.
 300  CALL BLCDIV (IRET)
C
 999  RETURN
      END
      SUBROUTINE BLCDIV (IRET)
C-----------------------------------------------------------------------
C   BLCDIV divides multisource data in a scratch file  by the
C   calibrator flux densities given in the source table; if 0, 1.0 is
C   used.  If all calibrator flux densities are 1.0 then no operation
C   is performed.
C   Input from common:
C    NSOUWD        I    Number of sources included or excluded; if
C                       0 all sources are included.
C    DOSWNT        L    If .TRUE. then sources in SOUWAN are included
C                       If .FALSE. then excluded.
C    SOUWAN(30)    I    The source numbers of sources included or
C                       excluded.
C    DISKIN        I    Disk number of the input multisource data file
C                       whose SU table is to be used.
C    CNOIN         I    Catalog slot number for SU file.
C    VISCNO        I    /CFILES/ number of the scratch file
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C   Note: also uses buffers, BUFF1, BUFF2, UBUFF, NXBUFF
C   Note: assumes that IF  is the most slowly variable axis.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4, IFILE*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LENVIS,
     *   ILENBU, KBIND, IBIND, I, J, IVIS, IOFF, NOVIS,
     *   SUKOLS(MAXSUC), SUNUMV(MAXSUC), IDSOU, QUAL, SULUN, IFNO, INDX,
     *   NVPIF,BO, VO, ISURNO, NUMSOU, LOOP, SUFQID
      LOGICAL   T, F
      DOUBLE PRECISION BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'BLCAL.INC'
      INCLUDE 'INCS:DSEL.INC'
      REAL      SFLUX(UVBFSL/2), XSFLUX
      DOUBLE PRECISION LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      REAL     FLUX(4,MAXIF)
      DATA SULUN, LUNI, LUNO /27, 16,17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (SOUWAN(1).LE.0) SOUWAN(1) = 1
C                                       Message
      WRITE (MSGTXT,2000)
      CALL MSGWRT (6)
      LENVIS = CATBLK(KINAX)
C                                       Open source (SU) table
      CALL SOUINI ('READ', NXBUFF, DISKIN, CNOIN, 1, CATUV, SULUN,
     *   NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Get number of sources
      NUMSOU = NXBUFF(5)
C                                       Read flux array
      DO 30 LOOP = 1,NUMSOU
         ISURNO = LOOP
         CALL TABSOU ('READ', NXBUFF, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Get flux densities
         INDX = (IDSOU-1) * NUMIF
         DO 25 I = 1,NUMIF
            SFLUX(INDX+I) = FLUX(1,I)
C                                       All sources selected must have
C                                       flux densities - test that only
C                                       when data are read
            IF (SFLUX(INDX+I).GT.1.0E-10) SFLUX(INDX+I) = 1.0 /
     *         SFLUX(INDX+I)
 25         CONTINUE
 30      CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ISURNO, BUFF1, NXBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C                                       Do divisions
      NOVIS = (LREC - NRPARM) / LENVIS
      NVPIF = NOVIS / NUMIF
      IF (NVPIF.LT.1) NVPIF = 1
C                                       Open and init for write
C                                       visibility file
      CALL ZPHFIL ('SC', SCRVOL(VISCNO), SCRCNO(VISCNO), 1, IFILE,
     *   IRET)
      CALL ZOPEN (LUNO, INDO, SCRVOL(VISCNO), IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1040) IRET, 'WRIT'
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IPTRO = KBIND
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET, 'WRIT'
         GO TO 990
         END IF
C                                       Open and init for read
C                                       visibility file
      CALL ZOPEN (LUNI, INDI, SCRVOL(VISCNO), IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1040) IRET, 'READ'
         GO TO 990
         END IF
C                                       Init vis file for read
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET, 'READ'
         GO TO 990
         END IF
C                                       Loop
C                                       Read vis. record.
 100     CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET, 'READ'
            GO TO 990
            END IF
         IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
         IDSOU = SOUWAN(1)
C                                       loop thru buffer full
         DO 180 I = 1,INIO
            DO 120 J = 1,LREC
               BUFF2(IPTRO+J-1) = BUFF1(IPTRI+J-1)
 120           CONTINUE
            IF (ILOCSU.GE.0) IDSOU = BUFF2(IPTRO+ILOCSU) + 0.5
            IOFF = NRPARM
C                                       This assumes that IF is the
C                                       most slowly varying axis.
            DO 140 IVIS = 1,NOVIS
               IFNO = ((IVIS-1) / NVPIF) + 1
               INDX = (IDSOU-1) * NUMIF + IFNO
               XSFLUX = SFLUX(INDX)
               IF (XSFLUX.LE.1.E-10) THEN
                  WRITE (MSGTXT,1120) IDSOU
                  MSGTXT = 'FLUX NOT DEFINED FOR SOURCE'
                  IRET = 99
                  GO TO 990
                  END IF
               BUFF2(IPTRO+IOFF) = BUFF2(IPTRO+IOFF) * XSFLUX
               BUFF2(IPTRO+IOFF+1) = BUFF2(IPTRO+IOFF+1) * XSFLUX
               IOFF = IOFF + LENVIS
 140           CONTINUE
            IPTRI = IPTRI + LREC
            IPTRO = IPTRO + LREC
 180        CONTINUE
C                                       Write vis. record.
         CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, INIO, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET, 'WRIT'
            GO TO 990
            END IF
         IPTRO = KBIND
         GO TO 100
C                                       Done
C                                       Flush buffer
 200  INIO = 0
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, INIO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET, 'FLSH'
         GO TO 990
         END IF
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1210) IRET, 'READ'
         GO TO 990
         END IF
      CALL ZCLOSE (LUNO, INDO, IRET)
      IF (IRET.EQ.0) GO TO 999
         WRITE (MSGTXT,1210) IRET, 'WRIT'
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLCDIV: ERROR',I3,' OPENING SOURCE TABLE')
 1010 FORMAT ('BLCDIV: ERROR',I3,' READING SOURCE TABLE')
 1030 FORMAT ('BLCDIV: ERROR',I3,' CLOSING SOURCE TABLE')
 1040 FORMAT ('BLCDIV: ERROR',I3,' OPEN-FOR-',A4,' VIS FILE')
 1050 FORMAT ('BLCDIV: ERROR',I3,' INIT-FOR-',A4,' VIS FILE')
 1100 FORMAT ('BLCDIV: ERROR',I3,1X,A4,'ING VIS FILE')
 1120 FORMAT ('BLCDIV: FLUX NOT DEFINED FOR SOURCE',I4)
 1210 FORMAT ('BLCDIV: ERROR',I3,'CLOSING ',A4,' VIS FILE')
 2000 FORMAT ('Dividing data by source flux densities')
      END
      SUBROUTINE BLCHIS
C-----------------------------------------------------------------------
C   BLCHIS updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12, HILINE*72
      INTEGER   LUN2, IERR, I, TIME(3), DATE(3)
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'BLCAL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN2 /28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Open old history
      CALL HIOPEN (LUN2, DISKIN, CNOIN, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Add selection/calibration
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Bl table version written.
      WRITE (HILINE,3020) TSKNAM, BLVERO
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Write control info.
C                                       CC tables
      IF (DOMODL .AND. (XSMOD(1).LE.0.0)) THEN
C                                       CC File Name etc.
         CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN2, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 190
C                                        CCfile version no.
         WRITE (HILINE,2001) TSKNAM, CCTVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                        Number of images
         WRITE (HILINE,2002) TSKNAM, MFIELD
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Number of CLEAN components.
         DO 140 I = 1,MFIELD
            WRITE (HILINE,2003) TSKNAM, I, NCOMP(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
 140        CONTINUE
         END IF
C                                       Point source model
      IF (XSMOD(1).GT.0.0) THEN
         WRITE (HILINE,2020) TSKNAM, XSMOD(1), XSMOD(2), XSMOD(3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         END IF
C                                       Solution time
      XSOLIN = XSOLIN * (24.0 * 60.0)
      IF (XSOLIN.LT.1.0E3) THEN
         WRITE (HILINE,2025) TSKNAM, XSOLIN
      ELSE
         WRITE (HILINE,2026) TSKNAM
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Already divided by model
      IF (BPARM(1).GT.0.0) THEN
         WRITE (HILINE,2021) TSKNAM
      ELSE
         WRITE (HILINE,2022) TSKNAM
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Close HI file
 190  CALL HICLOS (LUN2, T, BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2001 FORMAT (A6,'INVER = ',I5,' /CC file version no.')
 2002 FORMAT (A6,'NMAPS =',I4,' /Number of CLEAN images used')
 2003 FORMAT (A6,'NCOMP(',I3,') = ',I8,' /Number of CLEAN comps.')
 2020 FORMAT (A6,'SMODEL = ',2(F10.5,','),F10.5,
     *   ' /Pt. model parameters')
 2021 FORMAT (A6,'BPARM(1) = 1 /Data already divided by model')
 2022 FORMAT (A6,'BPARM(1) = 0 /Divide data by model')
 2025 FORMAT (A6,'SOLINT=',F7.2,' /Soln. inter. (min)')
 2026 FORMAT (A6,'SOLINT = 0 / Average over all time')
 3020 FORMAT (A6,'BASEVER = ',I3,' /BL table written')
      END
      SUBROUTINE CHWANT (NCH, NIF, CHNSEL, CHFLGS)
C-----------------------------------------------------------------------
C   Makes a mask of the desired channels
C   Inputs:
C      NCH      I            Number spectral chans
C      NIF      I            Number IFs
C      CHNSEL   I(3,20,*)    Start, stop, incr 20 sets per IF
C   Outputs
C      CHFLGS   R(*,*)       1.0 => use, 0.0 => don't use
C-----------------------------------------------------------------------
      INTEGER   NCH, NIF, CHNSEL(3,20,*)
      REAL      CHFLGS(NCH,NIF)
C
      INTEGER   I, J, K
C-----------------------------------------------------------------------
      J = NCH * NIF
      CALL RFILL (J, 0.0, CHFLGS)
      DO 30 K = 1,NIF
         DO 20 J = 1,20
            IF ((CHNSEL(1,J,K).GT.0) .AND. (CHNSEL(3,J,K).GT.0) .AND.
     *         (CHNSEL(2,J,K).GE.CHNSEL(1,J,K))) THEN
               DO 10 I = CHNSEL(1,J,K),CHNSEL(2,J,K),CHNSEL(3,J,K)
                  CHFLGS(I,K) = 1.0
 10               CONTINUE
               END IF
 20         CONTINUE
 30      CONTINUE
C
 999  RETURN
      END
