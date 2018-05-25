LOCAL INCLUDE 'BLCHN.INC'
C                                                         Include BLCHN
C                                       Local include for BLCHN
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   CATIN(256), SEQIN, SEQ2, DISKIN, DISK2, CNOIN, CNOIN2,
     *   CCTVER, VISDSK, VISCNO, JBUFSZ, NCOMP(MAXFLD), IRNOBL,
     *   CHNSEL(3,20,MAXIF), SEQO, DISKO, CNOO, SUPRAD, BDVERO
      LOGICAL   SINGLE, DOMODL
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAME2(3), XCLAS2(2), XNAMEO(3),
     *   XCLASO(2), XXSOUR(4,30), XMETH, XCMOD
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAS2*6, NAMEO*12,
     *   CLASO*6, XSOUR(30)*16, CMETH*4, CMOD*4
      REAL   XSI, XDI, XSO, XDO, XBLV, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XFLAG, XDOCAL, XGUSE, XDOPOL, XPDVER, XDOBND, XBPVER,
     *   XSMOTH(3), XS2, XD2, XVER, XNCOMP(MAXAFL), XFLUX, XNMAP,
     *   XSMOD(7), XCHNS(4,20), DOSCAL, XSPEC, XCURVE(3), BPARM(10),
     *   XBADD(10), BUFF1(UVBFSS), BUFF2(UVBFSS), SMOTAB(256), ACURVE(4)
      COMMON /CINFO/ CATIN, NCOMP, IRNOBL, SINGLE, DOMODL, CNOIN,
     *   CNOIN2, CCTVER, VISDSK, VISCNO, DISKIN, DISK2, SEQIN, SEQ2,
     *   CHNSEL, SEQO, DISKO, CNOO, SMOTAB, SUPRAD, BDVERO, ACURVE
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /XINPUT/ XNAMEI, XCLAIN, XSI, XDI, XNAMEO, XCLASO, XSO,
     *   XDO, XXSOUR, XTIME, XBAND, XFREQ, XFQID, XSUBA, XFLAG, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XDOBND, XBPVER, XBLV, XSMOTH, XNAME2,
     *   XCLAS2, XS2, XD2, XVER, XNCOMP, XFLUX, XNMAP, XMETH, XCMOD,
     *   XSMOD, XCHNS, DOSCAL, XSPEC, XCURVE, BPARM, XBADD
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAME2, CLAS2, NAMEO, CLASO, XSOUR,
     *   CMETH, CMOD
C                                                          End BLCHN
LOCAL END
      PROGRAM BLCHN
C-----------------------------------------------------------------------
C! Determines correlator based offsets given a model of the source.
C# UV Calibration AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2009-2017
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
C   BLCHN computes baseline dependent correction and writes them into
C   an BD table.  BLCHN divides the specified data by a given model
C   and averages the data for a specified time to determine the
C   corrections.  At present only multiplicative errors are corrected.
C     The object is to read a file containing
C   data from a strong point source (or data divided by model) and
C   averaged over time (eg one visrec from each baseline).  That
C   data will be written in BL table file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, MAXANS, MAXIFS, MAXCHS, NEED, TNEED, I
      LONGINT   BPT
      INCLUDE 'BLCHN.INC'
      REAL      WORK(2)
      DOUBLE PRECISION DWORK
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      EQUIVALENCE (WORK, DWORK)
      DATA PRGM /'BLCHN '/
C-----------------------------------------------------------------------
C                                       Release AP memory
      CALL QRLSE
C                                       Get input parameters and
C                                       create output file if nec.
      CALL BLCIN (PRGM, MAXANS, MAXIFS, MAXCHS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get dynamic memory
      MAXCHS = MAXCHS * MAXIFS
      NEED = MAXANS * MAXANS * MAXCHS
      TNEED = (3 * NEED - 1) / 1024 + 1
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
C                                       Average data
      MSGTXT = 'Now average the gain values'
      CALL MSGWRT (2)
      CALL BLCAV (MAXANS, MAXIFS, MAXCHS, WORK(BPT+1), WORK(BPT+1+NEED),
     *   WORK(BPT+1+2*NEED), BDVERO, IRET)
      IF (IRET.NE.0) GO TO 980
C                                       copy and apply
      MSGTXT = 'Now apply the gains to the data and write out'
      CALL MSGWRT (2)
      CALL BLCOUT (MAXANS, MAXCHS, WORK(BPT+1), WORK(BPT+1+NEED), IRET)
      IF (IRET.NE.0) GO TO 980
C                                       History
      CALL BLCHIS
C                                       free memory
 980  CALL ZMEMRY ('FREE', TSKNAM, TNEED, WORK, BPT, I)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE BLCIN (PRGN, MAXANS, MAXIFS, MAXCHS, IRET)
C-----------------------------------------------------------------------
C   BLCIN gets input parameters for BLCHN and creates an output file
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
C   See prologue comments in BLCHN for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   MAXANS, MAXIFS, MAXCHS, IRET
C
      INCLUDE 'BLCHN.INC'
      INTEGER   IERR, NPARM, I, MXFLD, NUMSUB, IROUND, LUN1, NUMAN(513),
     *   LUN, NW(MAXIF), J, K, K1, K2, LT, LSPECT, N, FREQID
      LOGICAL   T, TABLE, EXIST, FITASC, MATCH
      REAL      CATR(128), CATINR(128), WIDTHS(4), SUPS(4), FX, X, W
      CHARACTER STAT*4, UTYPE*2
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DCHND.INC'
      EQUIVALENCE (NUMAN, BUFF2)
      EQUIVALENCE (CATBLK, CATR, CATH)
      EQUIVALENCE (CATIN, CATINR)
      DATA LUN1/28/
      DATA T /.TRUE./
      DATA WIDTHS /4.0, 2.0, 2.0, 3.0/
      DATA SUPS /1.0, 3.0, 1.0, 4.0/
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
      NPARM = 280 + MXFLD
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
      SEQO = IROUND (XSO)
      DISKIN = IROUND (XDI)
      DISK2 = IROUND (XD2)
      DISKO = IROUND (XDO)
      CCTVER = IROUND (XVER)
      CCTVER = MAX (0, CCTVER)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAMEO, NAMEO)
      CALL H2CHR (6, 1, XCLASO, CLASO)
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
      MAXCHS = ECHAN
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
C                                       use all channels
      DO 50 K = 1,MAXIF
         IF (NW(K).LE.0) THEN
            NW(K) = 1
            CHNSEL(1,1,K) = 1
            CHNSEL(2,1,K) = ECHAN
            CHNSEL(3,1,K) = 1
            END IF
         DO 45 I = 1,NW(K)
            CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), ECHAN))
            IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K))
     *         CHNSEL(2,I,K) = ECHAN
            CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), ECHAN))
 45         CONTINUE
 50      CONTINUE
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
      BLVER = IROUND (XBLV)
C                                       See if do divide by model
      DOMODL = BPARM(1).LT.0.5
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
C                                       frequencies
      CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, 1, CATBLK, LUN, NUMIF,
     *   FOFF, ISBAND, FINC, BNDCOD, FREQID, IRET)
C                                       Number of IFs
      NUMIF = 1
      IF (JLOCIF.GE.0) NUMIF = CATBLK(KINAX+JLOCIF)
      MAXIFS = NUMIF
C                                       Number of polarizations
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
C                                       check smoothing
      LT = IROUND (BPARM(3))
      IF (LT.GT.0.0) THEN
         LT = MOD (LT-1, 4) + 1
         LSPECT = MAX (12, ECHAN)
         IF ((BPARM(4).LT.0.5) .OR. (BPARM(4).GT.LSPECT/3.))
     *      BPARM(4) = WIDTHS(LT)
         IF ((BPARM(5).GT.4.*SUPS(LT)*BPARM(4)) .OR.
     *      (BPARM(5).LT.BPARM(4))) BPARM(5) = SUPS(LT) * BPARM(4)
         SUPRAD = BPARM(5) / 2.0 + 0.1
         IF (SUPRAD+1.GT.MAXSMO) THEN
            SUPRAD = MAXSMO - 1
            BPARM(4) = (2. * SUPRAD) / SUPS(LT)
            END IF
         BPARM(5) = 2.0 * SUPRAD + 1.0
         N = 1 + SUPRAD
         FX = 2.0 / BPARM(5)
         SMOTAB(1) = 1.0
C                                       Compute look-up tables
         W = SMOTAB(1)
C                                       Hanning smooth
         IF (LT.EQ.1) THEN
            DO 60 I = 2,N
               X = I - 1.0
               SMOTAB(I) = MAX (0.0, 1.0-FX*X)
               W = W + 2 * SMOTAB(I)
 60            CONTINUE
C                                       Gaussian smooth
         ELSE IF (LT.EQ.2) THEN
            FX = -LOG(2.0) * FX * FX
            DO 65 I = 2,N
               X = I - 1.0
               SMOTAB(I) = EXP (FX * X * X)
               W = W + 2 * SMOTAB(I)
 65            CONTINUE
C                                       Boxcar smooth
         ELSE IF (LT.EQ.3) THEN
            FX = 1.0 / FX
            DO 70 I = 2,N
               X = I - 1.0
               IF (X.LT.FX) THEN
                  SMOTAB(I) = 1.0
               ELSE IF (X.EQ.FX) THEN
                  SMOTAB(I) = 0.5
                  END IF
               W = W + 2 * SMOTAB(I)
 70            CONTINUE
C                                      Sinc smooth
         ELSE IF (LT.EQ.4) THEN
            FX = 3.14159 * FX
            DO 75 I = 2,N
               X = (I - 1.0) * FX
               SMOTAB(I) = SIN(X) / X
               W = W + 2 * SMOTAB(I)
 75            CONTINUE
            END IF
C                                       Normalize integral
         IF (W.LE.0.0) W = 1.0
         DO 80 I = 1,N
            SMOTAB(I) = SMOTAB(I) / W
 80         CONTINUE
         END IF
      BPARM(3) = LT
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
      SUBROUTINE BLCAV (MAXANS, MAXIFS, MAXF, SUMP1, SUMP2, SWT, VER,
     *   IRET)
C-----------------------------------------------------------------------
C   BLCAV averages data and writes the BL table.
C   Input:
C      MAXANS   I      Number of antennas (array dimension)
C      MAXIFS   I      Number of IFs (array dimension)
C      MAXF     I      Number frequency channels times number IFs
C   Output:
C      SUMP1    R(*)   Work array. Sums for polarization 1
C                      (IF,ant1,ant2) two halves for real and imag.
C      SUMP2    R(*)   Work array. Sums for polarization 2
C      SWT      R(*)   Work array. sum weight
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   MAXANS, MAXIFS, MAXF, VER, IRET
      REAL      SUMP1(MAXF,MAXANS,MAXANS), SUMP2(MAXF,MAXANS,MAXANS),
     *   SWT(MAXF,MAXANS,MAXANS)
C
      INCLUDE 'BLCHN.INC'
      CHARACTER TYPE*2, IFILE*48
      INTEGER   INIO, IPTRI, LUNI, NFREQ, INDEX, IOFF, INDI, IDSOU, J,
     *   ILENBU, IBIND, I, IA1, IA2, LPIF, LPFRQ, VDSK, VCNO, BO, VO,
     *   NUMVIS, NUGOOD, II, NTAV, BDBUFF(512), IBDRNO, BDKOLS(10), JJ,
     *   BDNUMV(10)
      REAL      TMSRT, TIMEND, VISRE, VISIM, WT, CHFLGS(MAXCIF), TAV,
     *   BNDPAS(2*MAXCIF)
      LOGICAL   T, F
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
      CALL CHWANT (ECHAN, MAXIFS, CHNSEL, CHFLGS)
C                                       Initialize arrays.
      NUGOOD = 0
      DO 30 IA2 = 1,MAXANS
         DO 20 IA1 = 1,MAXANS
            DO 10 I = 1,MAXF
               SUMP1(I,IA1,IA2) = 0.0
               SUMP2(I,IA1,IA2) = 0.0
               SWT(I,IA1,IA2) = 0.0
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
         WRITE (MSGTXT,1000) IRET, 'OPENING DIVIDED FILE'
         GO TO 999
         END IF
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INITING DIVIDED FILE'
         GO TO 990
         END IF
      NUMVIS = 0
      TAV = 0.0
      NTAV = 0
C                                       Loop
C                                       Read vis. record.
 100     CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING DIVIDED FILE'
            GO TO 990
            END IF
         IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
C                                       Set initial time range.
         IF (NUMVIS.EQ.0) TMSRT = BUFF1(IPTRI+ILOCT)
         IF (NUMVIS.EQ.0) TIMEND = BUFF1(IPTRI+ILOCT) + 1.E6
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
C                                       Sum data
            NUGOOD = NUGOOD + 1
            NTAV = NTAV + 1
            TAV = TAV + BUFF1(IPTRI+ILOCT)
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
                     SUMP1(II,IA1,IA2) = SUMP1(II,IA1,IA2) +
     *                  VISRE * WT
                     SUMP1(II,IA2,IA1) = SUMP1(II,IA2,IA1) +
     *                  VISIM * WT
                     SWT(II,IA1,IA2) = SWT(II,IA1,IA2) + WT
                     END IF
C                                       Get data pointer 2nd poln.
                  INDEX = INDEX + INCS
                  VISRE = BUFF1(INDEX)
                  VISIM = BUFF1(INDEX+1)
                  WT = BUFF1(INDEX+2) * CHFLGS(II)
C                                       Sum second polarization.
                  IF ((WT.GT.1.0E-20) .AND. (NUMPOL.GT.1)) THEN
                     SUMP2(II,IA1,IA2) = SUMP2(II,IA1,IA2) +
     *                  VISRE * WT
                     SUMP2(II,IA2,IA1) = SUMP2(II,IA2,IA1) +
     *                  VISIM * WT
                     SWT(II,IA2,IA1) = SWT(II,IA2,IA1) + WT
                     END IF
 130              CONTINUE
 140           CONTINUE
C                                       Increment buffer pointer
            IPTRI = IPTRI + LREC
 190     CONTINUE
      GO TO 100
C                                       Only call
 200  IF (NUGOOD.GT.0) CALL BLCAVG (MAXANS, MAXF, SUMP1, SUMP2, SWT,
     *   NUGOOD)
      IF (NUGOOD.LE.0) THEN
         MSGTXT = 'NO GOOD SOLUTIONS FOUND'
         IRET= 10
         GO TO 990
         END IF
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING DIVIDED FILE'
         GO TO 990
         END IF
C                                       spectral index correction
      CALL FIXSPX (MAXANS, MAXIFS, MAXF, SUMP1, SUMP2)
C                                       write BD table
      CALL CATFIX (DISKIN, CNOIN, 'NOTR')
      VER = 0
      CALL BDINI ('WRIT', BDBUFF, DISKIN, CNOIN, VER, CATIN, LUNI,
     *   IBDRNO, BDKOLS, BDNUMV, MAXANS, NUMPOL, NUMIF, NFREQ, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING BD TABLE'
         GO TO 990
         END IF
      TAV = TAV / MAX (1, NTAV)
      DO 260 IA1 = 1,MAXANS
         DO 250 IA2 = IA1+1,MAXANS
            II = 0
            JJ = -1
            NUGOOD = 0
            DO 220 J = 1,NUMIF
               DO 210 I = 1,NFREQ
                  II = II + 1
                  JJ = JJ + 2
                  IF ((SUMP1(II,IA1,IA2).EQ.0.0) .AND.
     *               (SUMP1(II,IA2,IA1).EQ.0.0)) THEN
                     BNDPAS(JJ) = FBLANK
                     BNDPAS(JJ+1) = FBLANK
                  ELSE IF ((SUMP1(II,IA1,IA2).EQ.FBLANK) .OR.
     *               (SUMP1(II,IA2,IA1).EQ.FBLANK)) THEN
                     BNDPAS(JJ) = FBLANK
                     BNDPAS(JJ+1) = FBLANK
                  ELSE
                     BNDPAS(JJ) = SUMP1(II,IA1,IA2)
                     BNDPAS(JJ+1) = SUMP1(II,IA2,IA1)
                     NUGOOD = NUGOOD + 1
                     END IF
 210              CONTINUE
 220           CONTINUE
            II = 0
            IF (NUMPOL.GT.1) THEN
               DO 240 J = 1,NUMIF
                  DO 230 I = 1,NFREQ
                     II = II + 1
                     JJ = JJ + 2
                     IF ((SUMP2(II,IA1,IA2).EQ.0.0) .AND.
     *                  (SUMP2(II,IA2,IA1).EQ.0.0)) THEN
                        BNDPAS(JJ) = FBLANK
                        BNDPAS(JJ+1) = FBLANK
                     ELSE IF ((SUMP2(II,IA1,IA2).EQ.FBLANK) .OR.
     *                  (SUMP2(II,IA2,IA1).EQ.FBLANK)) THEN
                        BNDPAS(JJ) = FBLANK
                        BNDPAS(JJ+1) = FBLANK
                     ELSE
                        BNDPAS(JJ) = SUMP2(II,IA1,IA2)
                        BNDPAS(JJ+1) = SUMP2(II,IA2,IA1)
                        NUGOOD = NUGOOD + 1
                        END IF
 230                 CONTINUE
 240              CONTINUE
               END IF
            IF (NUGOOD.GT.0) THEN
               CALL TABBD ('WRIT', BDBUFF, IBDRNO, BDKOLS, BDNUMV,
     *            NUMIF, NFREQ, NUMPOL, TAV, IDSOU, SUBARR, IA1, IA2,
     *            FRQSEL, BNDPAS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING BD TABLE'
                  GO TO 990
                  END IF
               END IF
 250        CONTINUE
 260     CONTINUE
      CALL TABBD ('CLOS', BDBUFF, IBDRNO, BDKOLS, BDNUMV, NUMIF, NFREQ,
     *   NUMPOL, TAV, IDSOU, SUBARR, IA1, IA2, FRQSEL, BNDPAS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING BD TABLE'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLCAV: ERROR',I3,1X,A)
      END
      SUBROUTINE BLCAVG (MAXANS, MAXF, SUMP1, SUMP2, SWT, NGOOD)
C-----------------------------------------------------------------------
C   BLCAVG computes averages
C   Inputs:
C      MAXANS   I      Number of antennas (array dimension)
C      MAXF     I      Number of channs * IFs (array dimension)
C   Input/Output:
C      SUMP1    D()    Sum of visibilities 1st poln., (IF, ant1, ant2),
C                      Real part in ant1<ant2 and Imag part in
C                      ant2<ant1.   Zeroed on return.
C      SUMP2    D()    Sum of visibilities 2nd poln., zeroed on return.
C      SWT      R()    Sum WT, (IF, Ant1, Ant2) Poln 1 in Ant1<Ant2
C   Inputs from COMMON:
C      NUMANT   I      Number of antennas
C      NUMPOL   I      Number of polarizations
C      NUMIF    I      Number of IFs
C      SUBARR   I      Subarray number
C   Output:
C      NGOOD    I      Number good sol'ns
C-----------------------------------------------------------------------
      INTEGER   MAXANS, MAXF, NGOOD
      REAL      SWT(MAXF,MAXANS,MAXANS), SUMP1(MAXF,MAXANS,MAXANS),
     *   SUMP2(MAXF,MAXANS,MAXANS)
C
      INTEGER   IANT1, IANT2, LOOPF, LIMIT1, LIMIT2, II, LOOPIF
      REAL      RP1, RP2, IP1, IP2, XDIV, W1, W2, A, RS1, RS2, IS1, IS2,
     *   PRV(8)
      INCLUDE 'BLCHN.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      NGOOD = 0
      IF (BPARM(2).GT.0.0) THEN
         MSGTXT = 'Averaged solutions by baseline/IF'
         CALL MSGWRT (5)
         MSGTXT = 'A1 A2 IF  AMP1 ( SIG1 )  PHS1 (PSG1)' //
     *      '  AMP2 ( SIG2 )  PHS2 (PSG2)'
         CALL MSGWRT (5)
         END IF
C                                       Average data, convert to factors
      LIMIT1 = NUMANT - 1
      DO 100 IANT1 = 1,LIMIT1
         LIMIT2 = IANT1 + 1
         DO 90 IANT2 = LIMIT2,NUMANT
C                                       Loop over IF
            DO 30 LOOPIF = 1,NUMIF
               RP1 = 0.0
               RP2 = 0.0
               IP1 = 0.0
               IP2 = 0.0
               RS1 = 0.0
               RS2 = 0.0
               IS1 = 0.0
               IS2 = 0.0
               W1 = 0.0
               W2 = 0.0
               II = (LOOPIF-1) * ECHAN
               DO 10 LOOPF = 1,ECHAN
                  II = II + 1
C                                       Average 1st poln
                  IF (SWT(II,IANT1,IANT2).GT.0.0) THEN
                     W1 = W1 + SWT(II,IANT1,IANT2)
                     RP1 = RP1 + SUMP1(II,IANT1,IANT2)
                     IP1 = IP1 + SUMP1(II,IANT2,IANT1)
                     XDIV = 1.0D0 / SWT(II,IANT1,IANT2)
                     RS1 = RS1 + SUMP1(II,IANT1,IANT2) *
     *                  SUMP1(II,IANT1,IANT2) * XDIV
                     IS1 = IS1 + SUMP1(II,IANT2,IANT1) *
     *                  SUMP1(II,IANT2,IANT1) * XDIV
                     SUMP1(II,IANT1,IANT2) = SUMP1(II,IANT1,IANT2)*XDIV
                     SUMP1(II,IANT2,IANT1) = SUMP1(II,IANT2,IANT1)*XDIV
                     A = SUMP1(II,IANT1,IANT2)**2 +
     *                  SUMP1(II,IANT2,IANT1)**2
                     IF (A.GT.0.0) THEN
                        SUMP1(II,IANT1,IANT2) = SUMP1(II,IANT1,IANT2)/A
                        SUMP1(II,IANT2,IANT1) = -SUMP1(II,IANT2,IANT1)/A
                        END IF
                     NGOOD = NGOOD + 1
                     END IF
C                                       Average 2nd poln
                  IF (SWT(II,IANT2,IANT1).GT.0) THEN
                     W2 = W2 + SWT(II,IANT2,IANT1)
                     RP2 = RP2 + SUMP2(II,IANT1,IANT2)
                     IP2 = IP2 + SUMP2(II,IANT2,IANT1)
                     XDIV = 1.0D0 / SWT(II,IANT2,IANT1)
                     RS2 = RS2 + SUMP2(II,IANT1,IANT2) *
     *                  SUMP2(II,IANT1,IANT2) * XDIV
                     IS2 = IS2 + SUMP2(II,IANT2,IANT1) *
     *                  SUMP2(II,IANT2,IANT1) * XDIV
                     SUMP2(II,IANT1,IANT2) = SUMP2(II,IANT1,IANT2)*XDIV
                     SUMP2(II,IANT2,IANT1) = SUMP2(II,IANT2,IANT1)*XDIV
                     A = SUMP2(II,IANT1,IANT2)**2 +
     *                  SUMP2(II,IANT2,IANT1)**2
                     IF (A.GT.0.0) THEN
                        SUMP2(II,IANT1,IANT2) = SUMP2(II,IANT1,IANT2)/A
                        SUMP2(II,IANT2,IANT1) = -SUMP2(II,IANT2,IANT1)/A
                        END IF
                     NGOOD = NGOOD + 1
                     END IF
 10               CONTINUE
               CALL RFILL (8, 0.0, PRV)
               IF (W1.GT.0.0) THEN
                  RP1 = RP1 / W1
                  IP1 = IP1 / W1
                  RS1 = MAX (0.0, RS1 / W1 - RP1 * RP1)
                  IS1 = MAX (0.0, IS1 / W1 - IP1 * IP1)
                  A = RP1 * RP1 + IP1 * IP1
                  PRV(1) = A
                  IF (A.GT.0.0) THEN
                     PRV(2) = SQRT (RP1 * RP1 * RS1 + IP1 * IP1 * IS1)
     *                  / A
                     PRV(3) = RAD2DG * ATAN2 (IP1, RP1)
                     PRV(4) = SQRT (IP1 * IP1 * RS1 + RP1 * RP1 * IS1)
     *                  / (A * A) * RAD2DG
                     RP1 = RP1 / A
                     IP1 = -IP1 / A
                     END IF
                  END IF
               IF (W2.GT.0.0) THEN
                  RP2 = RP2 / W2
                  IP2 = IP2 / W2
                  RS2 = MAX (0.0, RS2 / W2 - RP2 * RP2)
                  IS2 = MAX (0.0, IS2 / W2 - IP2 * IP2)
                  A = RP2 * RP2 + IP2 * IP2
                  PRV(5) = A
                  IF (A.GT.0.0) THEN
                     PRV(6) = SQRT (RP2 * RP2 * RS2 + IP2 * IP2 * IS2)
     *                  / A
                     PRV(7) = RAD2DG * ATAN2 (IP2, RP2)
                     PRV(8) = SQRT (IP2 * IP2 * RS2 + RP2 * RP2 * IS2)
     *                  / (A * A) * RAD2DG
                     RP2 = RP2 / A
                     IP2 = -IP2 / A
                     END IF
                  END IF
               IF ((BPARM(2).GT.0.0) .AND. ((W1.GT.0.0) .OR.
     *            (W2.GT.0.0))) THEN
                  PRV(2) = MIN (9.99, PRV(2))
                  PRV(6) = MIN (9.99, PRV(6))
                  PRV(4) = MIN (99.9, PRV(4))
                  PRV(8) = MIN (99.9, PRV(8))
                  WRITE (MSGTXT,1010) IANT1, IANT2, LOOPIF, PRV
                  CALL MSGWRT (5)
                  END IF
               II = (LOOPIF-1) * ECHAN
               DO 20 LOOPF = 1,ECHAN
                  II = II + 1
C                                       substitute
                  IF (SWT(II,IANT1,IANT2).LE.0.0) THEN
                     SUMP1(II,IANT1,IANT2) = RP1
                     SUMP1(II,IANT2,IANT1) = IP1
                     END IF
C                                       substitute 2nd poln
                  IF (SWT(II,IANT2,IANT1).LE.0) THEN
                     SUMP2(II,IANT1,IANT2) = RP2
                     SUMP2(II,IANT2,IANT1) = IP2
                     END IF
 20               CONTINUE
               IF (BPARM(3).GT.0.0) THEN
                  II = (LOOPIF-1) * ECHAN + 1
                  CALL SOLSMO (SUPRAD, SMOTAB, ECHAN,
     *               SUMP1(II,IANT1,IANT2))
                  CALL SOLSMO (SUPRAD, SMOTAB, ECHAN,
     *               SUMP1(II,IANT2,IANT1))
                  CALL SOLSMO (SUPRAD, SMOTAB, ECHAN,
     *               SUMP2(II,IANT1,IANT2))
                  CALL SOLSMO (SUPRAD, SMOTAB, ECHAN,
     *               SUMP2(II,IANT2,IANT1))
                  END IF
 30            CONTINUE
 90         CONTINUE
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (I2,'-',I2,I3,2(F7.4,'(',F6.4,')',F7.2,'(',F4.2,')'))
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
      INCLUDE 'BLCHN.INC'
      INCLUDE 'INCS:DGDS.INC'
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
         CALL CHNCOP (IIVER, OOVER, LUN1, LUN2, DISKIN, SCRVOL(VISCNO),
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
      INTEGER   MODEL, METHOD, ISTOKE, DISKOU, ISCR, CHAN, NCHAN, I,
     *   IIF, IROUND
      LOGICAL   DOMSG, F, NONAM, NOCLAS, WASOME
      INCLUDE 'BLCHN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INTEGER   BITER(MAXFLD)
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
            CALL FACSET (DISKIN, CNOIN, 1, SOUWAN(1), MODEL, 1.0,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
      CNOIN2 = CCCNO(1)
C                                       Divide data by model
      DISKOU = VISDSK
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
               DISKOU = VISDSK
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
            CALL UVMDIV (VISDSK, VISCNO, DISKOU, ISCR, MODEL, METHOD,
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
         CALL UVMDIV (VISDSK, VISCNO, DISKOU, ISCR, MODEL, METHOD,
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
      INCLUDE 'BLCHN.INC'
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
C   BLCHIS write HI to new file and copies tables
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, NOTTYP(2)*2
      INTEGER   LUN1, LUN2, IERR, I, I1, I2, NONOT
      LOGICAL   T
      INCLUDE 'BLCHN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /2, 'NX','BD'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, CNOIN, CNOO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMEO, CLASO, SEQO, DISKO, LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Add selection/calibration
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       BD table version written
      WRITE (HILINE,3020) TSKNAM, BDVERO
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
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
C                                       average all times
      WRITE (HILINE,2026) TSKNAM
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
C                                        Spectral smoothing
      IF (BPARM(3).GT.0.5) THEN
         I1 = BPARM(3) + 0.5
         I2 = BPARM(5) + 0.5
         WRITE (HILINE,2040) TSKNAM, I1, BPARM(4), I2
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Close HI file
 190  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                       Copy tables
 200  CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, CNOIN,
     *   CNOO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR IN COPYING TABLES TO OUTPUT FILE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK
      CALL CATIO ('UPDT', DISKO, CNOO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' COPYING TO NEW HI FILE')
 2001 FORMAT (A6,'INVER = ',I5,' / CC file version no.')
 2002 FORMAT (A6,'NMAPS =',I4,' / Number of CLEAN images used')
 2003 FORMAT (A6,'NCOMP(',I3,') = ',I8,' / Number of CLEAN comps.')
 2020 FORMAT (A6,'SMODEL = ',2(F10.5,','),F10.5,
     *   ' /Pt. model parameters')
 2021 FORMAT (A6,'BPARM(1) = 1 / Data already divided by model')
 2022 FORMAT (A6,'BPARM(1) = 0 / Task divides data by model')
 2026 FORMAT (A6,'SOLINT = 0 / Data averaged over all time')
 2040 FORMAT (A6,'SOLSMOTH = ',I1,',',F6.1,',',I4,
     *   ' / Spectral smoothing after average')
 3020 FORMAT (A6,'BDVER = ',I5,' / BD table written')
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
      SUBROUTINE BLCOUT (MAXANS, MAXF, SUMP1, SUMP2, IRET)
C-----------------------------------------------------------------------
C   BLCOUT copies the input uv data set applying the solution
C   Inputs:
C      MAXANS   I      Array dimension # antennas
C      MAXF     I      Array dimension Nchans * Nifs
C      SUMP1    R(*)   Solution polarization 1
C      SUMP2    R(*)   Solution polarization 2
C   Outputs:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   MAXANS, MAXF, IRET
      REAL      SUMP1(MAXF,MAXANS,MAXANS), SUMP2(MAXF,MAXANS,MAXANS)
C
      INTEGER   ILOCWT, NCOPY, NCORO, ILENBU, VO, BO, LUNO, LUNI, INDI,
     *   INDO, KBIND, IPTRI, IPTRO, IBIND, NIOUT, NIOLIM, NUMVIS,
     *   RNXRET, INIO, I
      LOGICAL   ISCOMP, T, F
      CHARACTER BLANK*6, IFILE*48, OFILE*48
      INCLUDE 'BLCHN.INC'
      REAL      UBUFF(UVBFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA BLANK /' '/
      DATA T, F /.TRUE., .FALSE./
      DATA LUNI, LUNO /28,29/
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATBLK)
      CALL UVPGET (IRET)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMEO, CLASO, SEQO)
      CALL CHR2H (12, NAMEO, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASO, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQO
C                                       create output
      CALL UVCREA (DISKO, CNOO, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING OUTPUT UV DATA SET'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOO
      FRW(NCFILE) = 2
C                                       copy keywords
      CALL KEYCOP (DISKIN, CNOIN, DISKO, CNOO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'COPYING KEYWORDS'
         GO TO 990
         END IF
      SEQO = CATBLK(KIIMS)
      CALL UVPGET (IRET)
C                                       Find weight and scale.
      ISCOMP = CATBLK(KINAX).EQ.1
      IF (ISCOMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            IRET = 9
            GO TO 990
            END IF
         END IF
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, CNOIN, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN INPUT UV'
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CNOO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT UV'
         GO TO 990
         END IF
C                                       Init vis file for write
      NCOPY = LREC - NRPARM
      NCORO = (LREC - NRPARM) / CATBLK(KINAX)
      ILENBU = 0
      VO = 0
      BO = 1
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT OUTPUT UV'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT INPUT UV'
         GO TO 990
         END IF
      NUMVIS = 0
C                                       make an index table
      CALL RNXGET (DISKIN, CNOIN, CATIN)
      CALL RNXINI (DISKO, CNOO, CATBLK, RNXRET)
C                                       Loop
C                                       Read vis. record.
 100     CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', 'INPUT'
            GO TO 990
            END IF
         IPTRI = IBIND
         DO 190 I = 1,INIO
C                                       move random parameters
            CALL RCOPY (NRPARM, BUFF1(IPTRI), BUFF2(IPTRO))
C                                       compressed
            IF (ISCOMP) THEN
               CALL ZUVXPN (NCORO, BUFF1(IPTRI+NRPARM),
     *            BUFF1(IPTRI+ILOCWT), UBUFF)
               CALL SCALIT (MAXANS, MAXF, SUMP1, SUMP2, BUFF2(IPTRO),
     *            UBUFF, IRET)
               IF (IRET.EQ.0) CALL ZUVPAK (NCORO, UBUFF,
     *            BUFF2(IPTRO+ILOCWT), BUFF2(IPTRO+NRPARM))
C                                       Decide if it's kept, select
            ELSE
               CALL SCALIT (MAXANS, MAXF, SUMP1, SUMP2, BUFF2(IPTRO),
     *            BUFF1(IPTRI+NRPARM), IRET)
               IF (IRET.EQ.0) CALL RCOPY (NCOPY, BUFF1(IPTRI+NRPARM),
     *            BUFF2(IPTRO+NRPARM))
               END IF
            NUMVIS = NUMVIS + 1
            NIOUT = NIOUT + 1
C                                       update NX table
            CALL RNXUPD (BUFF2(IPTRO), RNXRET)
            IPTRI = IPTRI + LREC
            IPTRO = IPTRO + LREC
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRIT OUTPUT UV'
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
         IF (INIO.GT.0) GO TO 100
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH OUTPUT UV'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = NUMVIS
      CALL UCMPRS (NVIS, DISKO, CNOO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
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
 1000 FORMAT ('BLCOUT: ERROR',I4,1X,A)
      END
      SUBROUTINE SCALIT (MAXANS, MAXF, SUMP1, SUMP2, RPARM, VIS, IRET)
C-----------------------------------------------------------------------
C   SCALIT scales the vis by the correction factors
C   Inputs:
C      MAXANS   I      Array dimension # antennas
C      MAXF     I      Array dimension Nchans * Nifs
C      SUMP1    R(*)   Solution polarization 1
C      SUMP2    R(*)   Solution polarization 2
C      RPARM    R(*)   Random parameters
C   In/out:
C      VIS      R(*)   Visibilities
C   Output:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   MAXANS, MAXF, IRET
      REAL      SUMP1(MAXF,MAXANS,MAXANS), SUMP2(MAXF,MAXANS,MAXANS),
     *   RPARM(*), VIS(3,*)
C
      INTEGER   IA1, IA2, LIF, LCH, NFREQ, LNCIF, LNCF, LNCS, INCX,
     *   NUMPOL, NUMIF, II, IND
      REAL      CR, CI, TR, TI
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (ILOCB.GE.0) THEN
         IA2 = RPARM(1+ILOCB) + 0.01
         IA1 = IA2 / 256
         IA2 = IA2 - IA1*256
      ELSE
         IA1 = RPARM(1+ILOCA1) + 0.01
         IA2 = RPARM(1+ILOCA2) + 0.01
         END IF
      IF (JLOCIF.GT.0) THEN
         NUMIF = CATBLK(KINAX+JLOCIF)
      ELSE
         NUMIF  = 1
         END IF
      NFREQ = CATBLK(KINAX+JLOCF)
      INCX = CATBLK(KINAX)
      LNCIF = INCIF / INCX
      LNCF = INCF / INCX
      LNCS = INCS / INCX
      NUMPOL = CATBLK(KINAX+JLOCS)
C                                       cross-correlations only
C                                       first polarization
      IF (IA1.NE.IA2) THEN
         II = 0
         DO 20 LIF = 1,NUMIF
            IND = 1 + (LIF - 1) * LNCIF
            DO 10 LCH = 1,NFREQ
               II = II + 1
               IF (VIS(3,IND).GT.0.0) THEN
                  CR = SUMP1 (II, IA1, IA2)
                  CI = -SUMP1 (II, IA2, IA1)
                  TR = CR * VIS(1,IND) + CI * VIS(2,IND)
                  TI = CR * VIS(2,IND) - CI * VIS(1,IND)
                  VIS(1,IND) = TR
                  VIS(2,IND) = TI
                  CR = CR * CR + CI * CI
                  IF (CR.GT.0.0) THEN
                     VIS(3,IND) = VIS(3,IND) / CR
                  ELSE
                     VIS(3,IND) = 0.0
                     END IF
                  END IF
               IND = IND + LNCF
 10            CONTINUE
 20         CONTINUE
         END IF
C                                       second polarization
      IF ((IA1.NE.IA2) .AND. (NUMPOL.GT.1)) THEN
         II = 0
         DO 40 LIF = 1,NUMIF
            IND = 1 + (LIF - 1) * LNCIF + LNCS
            DO 30 LCH = 1,NFREQ
               II = II + 1
               IF (VIS(3,IND).GT.0.0) THEN
                  CR = SUMP2 (II, IA1, IA2)
                  CI = -SUMP2 (II, IA2, IA1)
                  TR = CR * VIS(1,IND) + CI * VIS(2,IND)
                  TI = CR * VIS(2,IND) - CI * VIS(1,IND)
                  VIS(1,IND) = TR
                  VIS(2,IND) = TI
                  CR = CR * CR + CI * CI
                  IF (CR.GT.0.0) THEN
                     VIS(3,IND) = VIS(3,IND) / CR
                  ELSE
                     VIS(3,IND) = 0.0
                     END IF
                  END IF
               IND = IND + LNCF
 30            CONTINUE
 40         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE SOLSMO (SUPRAD, SMOTAB, ECHAN, SPECT)
C-----------------------------------------------------------------------
C   Frequency smooth
C   Inputs:
C      SUPRAD   I      Support radius in channels
C      SMOTAB   R(*)   Smoothing function
C      ECHAN    I      Number channels
C   In/Out
C      SPECT    R(*)   Spectrum - in unsmoothed, out smoothed
C-----------------------------------------------------------------------
      INTEGER   SUPRAD, ECHAN
      REAL      SMOTAB(*), SPECT(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, J, J1, J2, L
      REAL      S, W, TEMP(MAXCHA)
C-----------------------------------------------------------------------
      DO 10 I = 1,ECHAN
         TEMP(I) = SPECT(I)
 10      CONTINUE
      DO 30 I = 1,ECHAN
         J1 = MAX (I-SUPRAD, 1)
         J2 = MIN (I+SUPRAD, ECHAN)
         S = 0.0
         W = 0.0
         DO 20 J = J1,J2
            L = ABS (I-J) + 1
            S = S + SMOTAB(L) * TEMP(J)
            W = W + SMOTAB(L)
 20         CONTINUE
         IF (W.GT.0.0) SPECT(I) = S / W
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FIXSPX (MAXANS, MAXIFS, MAXF, SUMP1, SUMP2)
C-----------------------------------------------------------------------
C   FIXSPX applies spectral-index correction if possible and not
C   forbidden
C   Input:
C      MAXANS   I      Number of antennas (array dimension)
C      MAXIFS   I      Number of IFs (array dimension)
C      MAXF     I      Number frequency channels times number IFs
C   In/Output:
C      SUMP1    R(*)   Work array. Sums for polarization 1
C                      (IF,ant1,ant2) two halves for real and imag.
C      SUMP2    R(*)   Work array. Sums for polarization 2
C-----------------------------------------------------------------------
      INTEGER   MAXANS, MAXIFS, MAXF
      REAL      SUMP1(MAXF,MAXANS,MAXANS), SUMP2(MAXF,MAXANS,MAXANS)
C
      INCLUDE 'BLCHN.INC'
      INTEGER   II, LIF, LF, IA1, IA2, NUMPOL
      REAL      FACT, FACTC
      DOUBLE PRECISION FF, FRQ, FRQ0, REFP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCHND.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       if allowed
      IF (DOSCAL.GE.0.0) THEN
         CALL RCOPY (3, XCURVE, ACURVE)
         ACURVE(4) = 0.0
C                                       default
         IF (XSPEC.EQ.0.0) CALL BLSPEC (DISKIN, CNOIN, DOSCAL, XSPEC,
     *      ACURVE)
C                                       is there something to do?
         IF ((XSPEC.NE.0.0) .OR. (ACURVE(1).NE.0.0)) THEN
            REFP = CATR(KRCRP+JLOCF)
            NUMPOL = CATBLK(KINAX+JLOCS)
            II = 0
            DO 100 LIF = 1,MAXIFS
               FRQ0 = CATD(KDCRV+JLOCF) + FOFF(LIF)
               FF = LOG10 (FRQ0 / 1.D9)
               FACTC = FF * XSPEC + (FF * FF) * ACURVE(1) +
     *            (FF * FF * FF) * ACURVE(2) + (FF ** 4) * ACURVE(3) +
     *            (FF ** 5) * ACURVE(4)
               FACTC = 10.0 ** (FACTC)
               DO 90 LF = 1,MAXF
                  II = II + 1
                  FRQ = FRQ0 + (LF - REFP) * FINC(LIF)
                  FF = LOG10 (FRQ / 1.D9)
                  FACT = FF * XSPEC + (FF * FF) * ACURVE(1) +
     *               (FF * FF * FF) * ACURVE(2) + (FF ** 4) * ACURVE(3)
     *               + (FF ** 5) * ACURVE(4)
                  FACT = 10.0 ** (FACT)
                  FACT = FACT / FACTC
                  DO 80 IA1 = 1,MAXANS
                     DO 70 IA2 = IA1+1,MAXANS
                        SUMP1(II,IA1,IA2) = SUMP1(II,IA1,IA2) * FACT
                        SUMP1(II,IA2,IA1) = SUMP1(II,IA2,IA1) * FACT
                        IF (NUMPOL.GT.1) THEN
                           SUMP2(II,IA1,IA2) = SUMP2(II,IA1,IA2) * FACT
                           SUMP2(II,IA2,IA1) = SUMP2(II,IA2,IA1) * FACT
                           END IF
 70                     CONTINUE
 80                  CONTINUE
 90               CONTINUE
 100           CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE BLSPEC (DISKIN, CNOIN, DOSCAL, XSPEC, ACURVE)
C-----------------------------------------------------------------------
C   BPSPEC checks the spectral index parameters in common and fills
C   in when the calsour is well known
C   Output
C      XSPEC    R      Spectral index - 0.0 means do none
C      ACURVE   R(3)   Spectral curvature parameters
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN
      REAL      DOSCAL, XSPEC, ACURVE(4)
C
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   XSOUR, NDATES, LXSOUR
      PARAMETER (XSOUR=5, NDATES=17, LXSOUR=6)
C
      INTEGER   IRET, LUNTMP, LUN, I, J, ISRC, ID(3), IDNUM, LSRC,
     *   ICTYPE, JTRIM, NTERM
      REAL      TCOEFF(4,XSOUR), DATES(NDATES), DCOEFF(4,NDATES,3), DD,
     *   W1, W2, LCOEFF(5,LXSOUR), SCOEFF(4,3), TEMP(3), PBOEFF(6,XSOUR)
      CHARACTER KNOSOU(4,XSOUR)*16, LNOSOU(4,LXSOUR)*16, DATE*8,
     *   SNOSOU(3,3)*16
      DOUBLE PRECISION DT
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATH, CATBLK)
C                                       Perley/Butler 2017
C                                       3C286
      DATA PBOEFF /1.2481, -0.4507, -0.1798, 0.0357, 0.0, 0.0,
C                                       3C48
     *             1.3253, -0.7553, -0.1914, 0.0498, 0.0, 0.0,
C                                       3C147
     *             1.4516, -0.6961, -0.2007, 0.0640, -0.0464, 0.0289,
C                                       3C138
     *             1.0088, -0.4981, -0.1552, -0.0102, 0.0223, 0.0,
C                                       3C295
     *             1.4701, -0.7658, -0.2780, -0.0347, 0.0399, 0.0/
C                                       steady sources Perley 2013
C                                       3C123
      DATA SCOEFF / 1.8077, -0.8018, -0.1157, 0.0,
C                                       3C196
     *              1.2969, -0.8690, -0.1788, 0.0305,
C                                       3C295
     *              1.4866, -0.7871, -0.3440, 0.0749/
C                                       Perley 2013 coefficients
C                                       same units as RCOEFF
C                                       3C286
      DATA TCOEFF /1.2515,  -0.4605,  -0.1715,   0.0336,
C                                       3C48 (2010)
     *             1.3197,  -0.7253,  -0.2023,   0.0540,
C                                       3C147 (2010)
     *             1.4428,  -0.6300,  -0.3142,   0.1032,
C                                       3C138 (2010)
     *             1.0053,  -0.4384,  -0.1855,   0.0511,
C                                       1934-638 (Reynolds, 02/Jul/94)
C    *           -30.7667,  26.4908,  -7.0977,   0.605334,
C                                       3C295
     *             1.4866,  -0.7871,  -0.3440,   0.0749 /
C                                       3C196
C    *             1.2969,  -0.8690,  -0.1788,   0.0305/
C                                       Source lists
      DATA KNOSOU /'3C286','1328+307','1331+305', 'J1331+3030',
     *   '3C48',    '0134+329', '0137+331', 'J0137+3309',
     *   '3C147',   '0538+498', '0542+498', 'J0542+4951',
     *   '3C138',   '0518+165', '0521+166', 'J0521+1638',
C    *   '1934-638','1934-638', '1934-638', 'J1939-6342',
     *   '3C295',   '1409+524', '1411+522', 'J1411+5212'/
C                                       date list
      DATA DATES /1983.4, 1985.9, 1987.3, 1989.9, 1995.2, 1998.1,
     *   1999.3, 2000.8, 2001.9, 2003.1, 2004.7, 2006.0, 2007.4, 2008.7,
     *   2010.0, 2010.9, 2012.0/
C                                       3C48
      DATA DCOEFF /
     *   1.3339,-.7643,-.1946,.055,   1.3350,-.7598,-.1869,.057,
     *   1.3361,-.7577,-.1905,.048,   1.3363,-.7605,-.1965,.057,
     *   1.3359,-.7673,-.2041,.059,   1.3342,-.7732,-.2078,.065,
     *   1.3342,-.7682,-.2097,.056,   1.3323,-.7654,-.2091,.060,
     *   1.3342,-.7708,-.2014,.059,   1.3341,-.7691,-.2006,.057,
     *   1.3341,-.7641,-.2102,.059,   1.3335,-.7705,-.2008,.058,
     *   1.3335,-.7660,-.1982,.051,   1.3361,-.7700,-.2119,.076,
     *   1.3334,-.7662,-.1988,.062,   1.3332,-.7665,-.1980,.064,
     *   1.3324,-.7690,-.1950,.059,
C                                       3C147
     *   1.4620,-.7085,-.2347,.051,   1.4648,-.7177,-.2501,.089,
     *   1.4624,-.7115,-.2336,.071,   1.4646,-.7194,-.2532,.092,
     *   1.4632,-.7121,-.2346,.086,   1.4641,-.7090,-.2313,.088,
     *   1.4642,-.7132,-.2424,.082,   1.4585,-.7086,-.2296,.068,
     *   1.4636,-.7124,-.2426,.084,   1.4639,-.7144,-.2453,.082,
     *   1.4635,-.7112,-.2453,.091,   1.4631,-.7136,-.2338,.094,
     *   1.4645,-.7115,-.2378,.084,   1.4625,-.7112,-.2396,.081,
     *   1.4623,-.7139,-.2405,.081,   1.4607,-.7150,-.2372,.077,
     *   1.4616,-.7187,-.2424,.079,
C                                       3C138
     *   1.0328,-.5523,-.1161,.008,   1.0337,-.5591,-.1605,.032,
     *   1.0354,-.5914,-.1032,-.005,  1.0292,-.5636,-.1857,.052,
     *   1.0145,-.5466,-.1758,.038,   1.0259,-.5679,-.1735,.039,
     *   1.0204,-.5702,-.1636,.030,   1.0081,-.5077,-.2492,.064,
     *   1.0196,-.5627,-.1823,.039,   1.0177,-.5686,-.1591,.029,
     *   1.0094,-.5003,-.2642,.085,   1.0181,-.5543,-.1486,.038,
     *   1.0149,-.5408,-.1174,.012,   1.0132,-.4941,-.1556,.045,
     *   1.0230,-.4983,-.1529,.048,   1.0207,-.5140,-.1626,.058,
     *   1.0332,-.5608,-.1197,.041/
C                                       Source lists: low freq
C                                       3C286
      DATA LCOEFF /27.477, -0.158,  0.032, -0.180,  0.000,
C                                       3C48
     *             64.768, -0.387, -0.420,  0.181,  0.000,
C                                       3C147
     *             66.738, -0.022, -1.012,  0.549,  0.000,
C                                       3C196
     *             83.084, -0.699,  0.110,  0.000,  0.000,
C                                       3c380
     *             77.352, -0.767,  0.000,  0.000,  0.000,
C                                       3C295
     *             97.763, -0.582, -0.298,  0.583, -0.363/
      DATA LNOSOU /'3C286','1328+307','1331+305', 'J1331+3030',
     *   '3C48',    '0134+329', '0137+331', 'J0137+3309',
     *   '3C147',   '0538+498', '0542+498', 'J0542+4951',
     *   '3C196',   '0809+483', '0813+482', 'J0813+482',
     *   '3C380',   '1828+487', '1829+487', 'J1829+487',
     *   '3C295',   '1409+524', '1411+522', 'J1411+5212'/
      DATA SNOSOU /'3C123', '0433+295', 'J0437+2946',
     *   '3C196', '0809+483', 'J0813+4813',
     *   '3C295', '1409+524', 'J1411+5212'/
C-----------------------------------------------------------------------
C                                       default - is source known?
      IF (XSPEC.EQ.0.0) THEN
         CALL RFILL (4, 0.0, ACURVE)
         LUN = LUNTMP (1)
         CALL GETSOU (SOUWAN(1), DISKIN, CNOIN, CATUV, LUN, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'SU TABLE PROBLEM,' //
     *         ' NO SPECTRAL INDEX DEFAULT'
            CALL MSGWRT (6)
         ELSE
            ICTYPE = 1
            IF (FREQ.LT.0.75D9) ICTYPE = -1
            CALL H2CHR (8, 1, CATH(KHDOB), DATE)
            CALL DATEST (DATE, ID)
            CALL DAYNUM (ID(1), ID(3), ID(2), IDNUM)
            DD = ID(1) + IDNUM/365.25
            IF (DD.GT.2014.0) ICTYPE = 0
            ISRC = 0
            IF (ICTYPE.EQ.1) THEN
               DO 10 I = 1,3
                  DO 5 J = 1,3
                     IF (SNAME(:JTRIM(SNOSOU(J,I))).EQ. SNOSOU(J,I))
     *                  ISRC = I + XSOUR
 5                   CONTINUE
 10               CONTINUE
               DO 20 I = 1,XSOUR
                  DO 15 J = 1,4
                     IF (SNAME(:JTRIM(KNOSOU(J,I))).EQ. KNOSOU(J,I))
     *                  ISRC = I
 15                  CONTINUE
 20               CONTINUE
            ELSE
               DO 40 I = 1,LXSOUR
                  DO 30 J = 1,4
                     IF (SNAME(:JTRIM(LNOSOU(J,I))).EQ. LNOSOU(J,I))
     *                  ISRC = I
 30                  CONTINUE
 40               CONTINUE
               END IF
C                                       non-standard source
            IF (ISRC.LE.0) THEN
               NTERM = 1
               IF (DOSCAL.GT.1.5) NTERM = 2
               CALL FNDSPX (DISKIN, CNOIN, SOUWAN(1), FRQSEL, CATUV,
     *            NTERM, TEMP, IRET)
               IF (IRET.EQ.0) THEN
                  XSPEC = TEMP(2)
                  IF (NTERM.EQ.2) ACURVE(1) = TEMP(3)
                  END IF
C                                       low frequency
            ELSE IF (ICTYPE.EQ.-1) THEN
C                                       return wrt 1 GHz, not 150 MHz
               DT = LOG10 (1.0D3 / 150.0D0)
               XSPEC = LCOEFF(2,ISRC) +
     *            2.D0*DT*LCOEFF(3,ISRC) +
     *            3.D0*DT*DT*LCOEFF(4,ISRC) +
     *            4.D0*DT*DT*DT * LCOEFF(5,ISRC)
               ACURVE(1) = LCOEFF(3,ISRC) +
     *            3.D0*DT*LCOEFF(4,ISRC) +
     *            6.D0*DT*DT*LCOEFF(5,ISRC)
               ACURVE(2) = LCOEFF(4,ISRC) +
     *            4.D0*DT*LCOEFF(5,ISRC)
               ACURVE(3) = LCOEFF(5,ISRC)
               WRITE (MSGTXT,1020) LNOSOU(1,ISRC), XSPEC, ACURVE(1),
     *            ACURVE(2), ACURVE(3)
               CALL MSGWRT (3)
C                                       Perley-Butler 2017
            ELSE IF (ICTYPE.EQ.0) THEN
               XSPEC = PBOEFF(2,ISRC)
               ACURVE(1) = PBOEFF(3,ISRC)
               ACURVE(2) = PBOEFF(3,ISRC)
               ACURVE(3) = PBOEFF(3,ISRC)
               ACURVE(4) = PBOEFF(3,ISRC)
               WRITE (MSGTXT,1020) KNOSOU(1,ISRC), XSPEC, ACURVE
               CALL MSGWRT (3)
C                                       stable ones
            ELSE IF (ISRC.GT.XSOUR) THEN
               ISRC = ISRC - XSOUR
               XSPEC = SCOEFF(2,ISRC)
               ACURVE(1) = SCOEFF(3,ISRC)
               ACURVE(2) = SCOEFF(4,ISRC)
               WRITE (MSGTXT,1020) SNOSOU(1,ISRC), XSPEC, ACURVE(1),
     *            ACURVE(2)
               CALL MSGWRT (3)
C                                       3C286, 3C295 stable
            ELSE IF ((ISRC.EQ.1) .OR. (ISRC.EQ.5)) THEN
               XSPEC = TCOEFF(2,ISRC)
               ACURVE(1) = TCOEFF(3,ISRC)
               ACURVE(2) = TCOEFF(4,ISRC)
               WRITE (MSGTXT,1020) KNOSOU(1,ISRC), XSPEC, ACURVE(1),
     *            ACURVE(2)
               CALL MSGWRT (3)
C                                       time variable
            ELSE IF (ISRC.GT.0) THEN
               LSRC = ISRC - 1
               IF ((DD.LE.DATES(1)) .OR. (DD.GE.DATES(NDATES))) THEN
                  I = NDATES
                  IF (DD.LE.DATES(1)) I = 1
                  XSPEC = DCOEFF(2,I,LSRC)
                  ACURVE(1) = DCOEFF(3,I,LSRC)
                  ACURVE(2) = DCOEFF(4,I,LSRC)
                  WRITE (MSGTXT,1020) KNOSOU(1,ISRC), XSPEC, ACURVE(1),
     *               ACURVE(2)
                  CALL MSGWRT (3)
C                                       interpolate
               ELSE
                  DO 50 I = 2,NDATES
                     IF (DD.LT.DATES(I)) THEN
                        W1 = (DATES(I) - DD) / (DATES(I) - DATES(I-1))
                        W2 = 1.0 - W1
                        XSPEC = W2 * DCOEFF(2,I,LSRC) +
     *                     W1 * DCOEFF(2,I-1,LSRC)
                        ACURVE(1) = W2 * DCOEFF(3,I,LSRC) +
     *                     W1 * DCOEFF(3,I-1,LSRC)
                        ACURVE(2) = W2 * DCOEFF(4,I,LSRC) +
     *                     W1 * DCOEFF(4,I-1,LSRC)
                        WRITE (MSGTXT,1020) KNOSOU(1,ISRC), XSPEC,
     *                     ACURVE(1), ACURVE(2)
                        CALL MSGWRT (3)
                        GO TO 999
                        END IF
 50                  CONTINUE
                  END IF
               END IF
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (A5,' default spectral index',5F7.3)
      END
