LOCAL INCLUDE 'LPCAL.INC'
C                                                         Include LPCAL
C                                       Local include for LPCAL
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXDAT, MAXCAL, MAXMOD
C                                       MAXDAT = Max number of vis in
C                                       arrays.
      PARAMETER (MAXDAT=40000)
C                                       MAXCAL = Number of calibrators
C                                       possible.
      PARAMETER (MAXCAL=1)
C                                       MAXMOD = Max number of models
C                                       in solution.
      PARAMETER (MAXMOD=10)

      INTEGER   CATIN(256), SEQIN, SEQ2, DISKIN, DISK2, CNOIN, CNOIN2,
     *   CCTVER, VISDSK, VISCNO, VMDSK, VMCNO, JBUFSZ, ANTS(2,MAXDAT),
     *   VSOU(MAXDAT), NVCAL, VCALID(MAXCAL), NCOMP(MAXAFL), NUMDAT,
     *   FREQID, NFIELD, NMODEL, NUMHIS, IBUFF1(UVBFSS), IBUFF2(UVBFSS)
      LOGICAL   SINGLE, CLNMOD, VDOFIT(MAXCAL), AVGIF
      REAL   XSI, XDI, XTIME(8), XBAND, XFREQ, XFQID, XBCHN, XECHN,
     *   XBIF, XEIF, XANTS(50), XUVRA(2), XSUBA, XDOCAL, XGUSE, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), XS2, XD2, XVER,
     *   XNCOMP(MAXAFL), XNMAP, XSOLIN, XPRTLV, CPARM(10), XBADD(10),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), TIME(MAXDAT), PARAN(2,MAXDAT),
     *   VMOD(2,MAXMOD,MAXDAT), VWT(4,MAXDAT), VOBS(2,4,MAXDAT),
     *   VFLUX(4,MAXCAL,MAXIF)
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XNAME2(3),
     *   XCLAS2(2), XCMETH, XCMOD
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, NAME2*12, CLAS2*6,
     *   XSOLTY*4, HISCRD(MAXMOD)*64, CMETH*4, CMOD*4
      COMPLEX CVOBS(4,MAXDAT), CVMOD(MAXMOD,MAXDAT)
      EQUIVALENCE (VOBS, CVOBS), (VMOD, CVMOD), (BUFF1, IBUFF1),
     *   (BUFF2, IBUFF2)
      COMMON /CINFO/ CATIN, NCOMP, SINGLE, CLNMOD, CNOIN, CNOIN2,
     *   CCTVER, VISDSK, VISCNO, VMDSK, VMCNO
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /XINPUT/ XNAMEI, XCLAIN, XSI, XDI, XXSOUR, XTIME, XBAND,
     *   XFREQ, XFQID, XBCHN, XECHN, XBIF, XEIF, XANTS, XUVRA, XSUBA,
     *   XDOCAL, XGUSE, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XNAME2,
     *   XCLAS2, XS2, XD2, XVER, XNCOMP, XNMAP, XCMETH, XCMOD, XSOLIN,
     *   XPRTLV, CPARM, XBADD
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, NAME2, CLAS2, XSOLTY,
     *   HISCRD, CMETH, CMOD
      COMMON /VDATA/ TIME, PARAN, VMOD, VWT, VOBS, VFLUX, VDOFIT,
     *   AVGIF, NUMDAT, ANTS, VSOU, NVCAL, VCALID, NFIELD, NMODEL,
     *   DISKIN, DISK2, SEQIN, SEQ2, FREQID, NUMHIS
C                                                          End LPCAL
LOCAL END
      PROGRAM LPCAL
C-----------------------------------------------------------------------
C! Determines Antenna polarization characteristics
C# UV Calibration AP-appl EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2001, 2003, 2006-2009, 2012, 2015-2016
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
C   LPCAL computes polarization parameters and enters them into the AN
C   table  also determines calibrator polarizations and enters them in
C   the SU table.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'LPCAL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      DATA PRGM /'LPCAL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL LPCIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Determine poln. parameters.
      CALL LPCUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History
      CALL LPCHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE LPCIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   LPCIN gets input parameters for LPCAL.
C   Inputs:
C      PRGN    C*6     Program name
C   Output:
C      IRET    I         Error code: 0 => ok
C                          1 => too few frequency channels.
C                          5 => catalog troubles
C                          8 => can't start
C   Commons: /XINPUT/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in LPCAL for more details.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, UTYPE*2, CLASS2*6, BLANK*12
      HOLLERITH CATH(256)
      INTEGER   IERR, I, I4T, NPARM, LOOPF, LOOPCC, NUMSUB, IROUND,
     *   LUN1, NUMAN(513), J0
      LOGICAL   T, TABLE, EXIST, FITASC, MATCH, OLDNAM
      REAL      CATR(256), CATINR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'LPCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (NUMAN, BUFF2)
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      EQUIVALENCE (CATIN, CATINR)
      DATA LUN1 /28/
      DATA T /.TRUE./
      DATA BLANK /'            '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
      DO3DIM = .FALSE.
      LIMFLX = -1.0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C
      NSOUWD = 0
C                                       Get input parameters.
      NPARM = 237 + MAXAFL
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
      AVGIF = CPARM(1).GT.0.001
      CLNMOD = CPARM(2).LT.0.001
      NFIELD = IROUND (XNMAP)
      NFIELD = MAX (1, MIN (MAXAFL, NFIELD))
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
      CALL H2CHR (4, 1, XCMETH, CMETH)
      CALL H2CHR (4, 1, XCMOD, CMOD)
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
C                                       Init. scratch file info.
      VISDSK = DISKIN
      VISCNO = CNOIN
      VMDSK = 0
      VMCNO = 0
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      DO 40 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 40      CONTINUE
C                                       Linearly polarized feeds?
      IF (CATD(KDCRV+JLOCS).LT.-4.0D0) THEN
         IRET = 4
         MSGTXT = 'CANNOT CALIBRATE LINEARLY POLARIZED FEEDS'
         GO TO 990
         END IF
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
      DO 80 I = 1,30
         SOURCS(I) = XSOUR(I)
         CALSOU(I) = XSOUR(I)
 80      CONTINUE
      CALL RCOPY (8, XTIME, TIMRNG)
      DXTIME = 1.0/86400.0
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      STOKES = 'FULL'
      BCHAN = IROUND (XBCHN)
      ECHAN = IROUND (XECHN)
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      XBIF = BIF
      XEIF = EIF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN1, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
      FREQID = FRQSEL
C                                       Antennas
      DO 85 I = 1,50
         ANTENS(I) = IROUND (XANTS(I))
 85      CONTINUE
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      FGVER = IROUND (XFLAG)
      CLVER = 0
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Preaverage interval
      IF (XSOLIN.LT.0.0) XSOLIN = 0.0
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
C                                       Number of IFs
      NUMIF = 1
      IF (JLOCIF.GE.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       Number of polarizations
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
C                                       Get antenna info
      CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get number of models
      IRET = 5
      IF (CLNMOD) THEN
         NMODEL = 0
C                                       No model == unpol. source???
         IF (NAME2.EQ.BLANK .OR. CLAS2.EQ.BLANK(1:6)) THEN
            MSGTXT = 'NO MODEL GIVEN - I ASSUME SOURCE IS UNPOLARIZED'
            CALL MSGWRT (6)
            GO TO 300
            END IF
         OLDNAM = (CLAS2(4:4).LT.'0') .OR. (CLAS2(4:4).GT.'9') .OR.
     *      (CLAS2(5:5).LT.'0') .OR. (CLAS2(5:5).GT.'9') .OR.
     *      (CLAS2(6:6).LT.'0') .OR. (CLAS2(6:6).GT.'9')
         IF (.NOT.OLDNAM) READ (CLAS2(4:6),1084) J0
         DO 240 LOOPF = 1,NFIELD
            NCOMP(LOOPF) = ABS (XNCOMP(LOOPF)) + 0.1
            IF (NCOMP(LOOPF).EQ.0) THEN
               CLASS2 = CLAS2
               IF (OLDNAM) THEN
                  I4T = LOOPF - 1
                  IF (LOOPF.NE.1) CALL ZEHEX (I4T, 2, CLASS2(5:6))
               ELSE
                  WRITE (CLASS2(4:6),1085) LOOPF-1+J0
                  END IF
               CNOIN2 = 1
               UTYPE = 'MA'
               CALL CATDIR ('SRCH', DISK2, CNOIN2, NAME2, CLASS2, SEQ2,
     *            UTYPE, NLUSER, STAT, BUFF1, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1030) IERR, NAME2, CLASS2, SEQ2, DISK2,
     *               NLUSER
                  GO TO 990
                  END IF
               DO 220 LOOPCC = CCTVER,CCTVER+MAXMOD-1
                  LUNS(1) = 29
                  CALL ISTAB ('CC', DISK2, CNOIN2, LOOPCC, LUNS, BUFF1,
     *               TABLE, EXIST, FITASC, IERR)
                  IF (EXIST .AND. (IERR.EQ.0)) THEN
                     NCOMP(LOOPF) = NCOMP(LOOPF) + 1
                  ELSE
                     GO TO 230
                     END IF
 220              CONTINUE
 230           END IF
            IF (NCOMP(LOOPF).EQ.0) THEN
               WRITE (MSGTXT,1230) NAME2, CLASS2, SEQ2, DISK2
               GO TO 990
               END IF
            NMODEL = NMODEL + NCOMP(LOOPF)
 240        CONTINUE
C                                       Use image model
      ELSE
C        NMODEL = NFIELD
         END IF
      IF (NMODEL.GT.MAXMOD) THEN
         WRITE (MSGTXT,1240) MAXMOD
         GO TO 990
         END IF
 300  IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LPCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I4)
 1050 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('SORT ORDER IS ',A2,' NOT ',A2,' AS REQUIRED')
 1084 FORMAT (I3)
 1085 FORMAT (I3.3)
 1160 FORMAT ('SPECIFIED SUBARRAY ',I4,' > MAX. OF ',I4)
 1230 FORMAT ('NO CC TABLES FOR ',A12,' . ',A6,' . ',I3,' DISK=',I3)
 1240 FORMAT ('TOO MANY MODELS ( > ',I3,')')
      END
      SUBROUTINE LPCUV (IRET)
C-----------------------------------------------------------------------
C   LPCUV fits model parameters and enters them into the AN table.
C   Output:
C    IRET        I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'LPCAL.INC'
      HOLLERITH CATINH(256)
      INTEGER   VERTMP, LOOP, LUNNEW, LUNOLD, I, BIFT, EIFT, MSGSAV,
     *   IFNO, LOOPA, IERR, PRTLV, IREF, IROUND, SOLTYP,
     *   LOOPIF, LIMIF1, LIMIF2, NIF, SAVEIF, SAVBIF
      LOGICAL   GETFLX, FITANT, SLCTD
      REAL      ORI(2,MAXIF,MAXANT), ELP(2,MAXIF,MAXANT)
      COMPLEX DDD(2,MAXANT)
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATIN, CATINH)
      DATA LUNNEW, LUNOLD /27,28/
C-----------------------------------------------------------------------
      IF (JLOCIF.GE.0) THEN
         NIF = CATIN(KINAX+JLOCIF)
      ELSE
         NIF = 1
         END IF
      SAVBIF = BIF
      SAVEIF = EIF
C                                        No reference antenna
      IREF = 0
C                                        Get previous solutions
      CALL GETANT (DISKIN, CNOIN, SUBARR, CATIN, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Save solutions
      DO 4 LOOPA = 1,MAXANT
         DO 2 LOOPIF = 1,MAXIF
            ORI(1,LOOPIF,LOOPA) = STNORI(1,LOOPIF,LOOPA)
            ORI(2,LOOPIF,LOOPA) = STNORI(2,LOOPIF,LOOPA)
            ELP(1,LOOPIF,LOOPA) = STNELP(1,LOOPIF,LOOPA)
            ELP(2,LOOPIF,LOOPA) = STNELP(2,LOOPIF,LOOPA)
 2          CONTINUE
 4       CONTINUE
C                                       Init. cal. fluxes etc.
      DO 10 LOOP = 1,MAXCAL
         VCALID(LOOP) = 0
         DO 10 I = 1,NUMIF
            VFLUX(1,LOOP,I) = 0.0
            VFLUX(2,LOOP,I) = 0.0
            VFLUX(3,LOOP,I) = 0.0
            VFLUX(4,LOOP,I) = 0.0
 10         CONTINUE
      IF (EIF.LE.0) EIF = NIF
      BIFT = BIF
      EIFT = EIF
      IF (AVGIF) EIFT = BIFT
      VISDSK = 0
      VISCNO = 0
C                                       Set up for common IF solution.
C                                       Loop over IF
      DO 500 IFNO = BIFT,EIFT
         IF (AVGIF) THEN
            LIMIF1 = BIF
            LIMIF2 = EIF
         ELSE
            LIMIF1 = IFNO
            LIMIF2 = IFNO
            END IF
C                                       Antenna list
         DO 90 LOOPA = 1,50
            ANTENS(LOOPA) = IROUND (XANTS(LOOPA))
 90         CONTINUE
C                                       Get data
         GETFLX = IFNO.LE.BIFT
         CALL LPCSEL (IFNO, GETFLX, IRET)
C                                       Check if any data:
         IF (IRET.EQ.-1) GO TO 500
         IF (IRET.NE.0) GO TO 999
         PRTLV = XPRTLV + 0.1
C                                       Feed solution
         SOLTYP = 4
         CALL DPCALC (NUMDAT, CVOBS, VWT, PARAN, ANTS,
     *      NUMANT, DDD, NMODEL, CVMOD, PRTLV, IRET)
C                                       Save Feed parameters
         DO 200 LOOPA = 1,NUMANT
            DO 190 LOOPIF = LIMIF1,LIMIF2
               ELP(1,LOOPIF,LOOPA) = REAL (DDD(1,LOOPA))
               ELP(2,LOOPIF,LOOPA) = REAL (DDD(2,LOOPA))
               ORI(1,LOOPIF,LOOPA) = AIMAG (DDD(1,LOOPA))
               ORI(2,LOOPIF,LOOPA) = AIMAG (DDD(2,LOOPA))
 190           CONTINUE
 200        CONTINUE
 500     CONTINUE
C                                       Rewrite An table
C                                       Find number of AN tables:
      CALL FNDEXT ('AN', CATBLK, VERTMP)
      IF (VERTMP.GT.0) VERTMP = VERTMP + 1
C                                        Save solutions
      DO 630 LOOPA = 1,MAXANT
C                                        Don't update antennas not fitted.
         FITANT = SLCTD (LOOPA, ANTENS, NANTSL, DOAWNT)
         IF (FITANT) THEN
            DO 620 LOOPIF = 1,MAXIF
               STNORI(1,LOOPIF,LOOPA) = ORI(1,LOOPIF,LOOPA)
               STNORI(2,LOOPIF,LOOPA) = ORI(2,LOOPIF,LOOPA)
               STNELP(1,LOOPIF,LOOPA) = ELP(1,LOOPIF,LOOPA)
               STNELP(2,LOOPIF,LOOPA) = ELP(2,LOOPIF,LOOPA)
 620           CONTINUE
            END IF
 630     CONTINUE
C                                       Copy to a temporary AN table.
C                                       Solutions to AN table
      CALL PUTANT (DISKIN, CNOIN, SUBARR, VERTMP, SAVBIF, SAVEIF, NIF,
     *   SOLTYP, CATIN, IBUFF1, IBUFF2, FREQID, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Delete old
      CALL RMEXT (DISKIN, CNOIN, 'AN', SUBARR, CATIN, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy back.
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('AN', VERTMP, SUBARR, LUNOLD, LUNNEW, DISKIN, DISKIN,
     *   CNOIN, CNOIN, CATIN, BUFF1, BUFF2, IRET)
      MSGSUP = MSGSUP
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1610) IRET
         GO TO 990
         END IF
C                                       Delete temporary
      CALL RMEXT (DISKIN, CNOIN, 'AN', VERTMP, CATIN, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1610 FORMAT ('TABCOP ERROR ',I3,' UPDATING AN TABLE')
      END
      SUBROUTINE LPCSEL (IFNO, GETFLX, IRET)
C-----------------------------------------------------------------------
C   LPCSEL will read a multi source data set into a common arrays.
C   Editing and calibration may be applied.
C   Input:
C      IFNO         I    IF number (only 1 at a time)
C      GETFLX       L    If true fill VFLUX from SU table.
C   Input via common:
C      XSOLIN       R    Solution interval (min)
C   Inputs via common /SELCAL/  (Include DSEL.INC)
C      UNAME        C    AIPS name of input file.
C      UCLAS        C    AIPS class of input file.
C      UDISK        R    AIPS disk of input file.
C      USEQ         R    AIPS sequence of input file.
C      SOURCS(30)   C    Names (16 char) of up to 30 sources, *=>all
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
C   Output in common:
C      NUMDAT       I    Number of visibilities in arrays:
C      TIME(*)      R    Times (days) of the visibilities
C      ANTS(2,*)    I    Antenna numbers
C      PARAN(2,*)   R    Parallactic angles (radians)
C      VMOD(2,MAXMOD,*)
C                   R    Complex total intensity models (Jy)
C      VOBS(2,4,*)  R    Complex observed values (RR,RL,LR,LL)  or
C                        (XX,XY,YX,YY) (Jy).
C      VWT(4,*)     R    "Weights" of the observations.
C      NVCAL        I    Number of calibrator sources
C      VCALID(*)    I    Calibrator ID numbers
C      VFLUX(4,*,if)R    Polarized flux densities (I,Q,U,V) (Jy)
C   Output:
C      IRET         I    Error code: 0 => OK,
C                        -1 => end of data
C                        >0 => failed, abort process.
C-----------------------------------------------------------------------
      INTEGER   IRET, IFNO
      LOGICAL   GETFLX
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   IA1, IA2, LUN1, LUN2, LSOU, SAVNRP, SAVLRC,
     *   NUMBL, IVIS, CNTBL, IND, ILENBU, KBIND, IPTRI, INIO,
     *   IOFF, SAVCR0, I, CSOUID, IMODEL
      LOGICAL   T, F
      INTEGER   MXDAT, LOOP, INDEX, BLNDX, NEXT, VO, BO, FIRST,
     *   LIMIT, LOPCNT, GATNDX(16), GNDX, LOOP2, IIVER, OOVER
      REAL     DUM, LTIMEP, XNORM,
     *   CTIME, TSCEND, SAVSTK, SOLINT, CATR(256)
      DOUBLE PRECISION CATD(128)
      CHARACTER IFILE*48
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'LPCAL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   SUMCNT(MXBASE), SUMPNT(MXBASE)
      REAL      PANGLE(MAXANT), VOBSX(8,MAXDAT)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSCD.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      EQUIVALENCE (VOBS, VOBSX)
      DATA T, F /.TRUE.,.FALSE./
      DATA VO, BO /0,1/
      DATA LUN1,LUN2 /27,28/
      DATA MXDAT /MAXDAT/
      DATA GATNDX /1,2,7,8,10,11,4,5, 13,14,16,17,19,20,22,23/
C-----------------------------------------------------------------------

C                                       Setup
      SOLINT = XSOLIN / (24.0 * 60.0)
      IF (IFNO.LE.0) IFNO = 1
      IF (AVGIF) THEN
         BIF = XBIF + 0.5
         EIF = XEIF + 0.5
      ELSE
         BIF = IFNO
         EIF = IFNO
         END IF
C                                       Message about IF number(s)
      IF (AVGIF) THEN
         WRITE (MSGTXT,1000) BIF, EIF
      ELSE
         WRITE (MSGTXT,1001) IFNO
         END IF
      CALL MSGWRT (6)
C                                       Get data
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy to scratch file.
      IF (NVIS.GT.0)
     *   CALL LPCCOP (VISDSK, VISCNO, BUFF1, JBUFSZ, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       No data - Bail out
      IF (NVIS.LE.0) THEN
         CALL UVGET ('CLOS', DUM, DUM, IRET)
         VISDSK = 0
         VISCNO = 0
         VMCNO = 0
         IRET = -1
         WRITE (MSGTXT,1100) IFNO
         GO TO 990
         END IF
C                                       Calibrators selected?
      IF (.NOT.SINGLE .AND. NSOUWD.EQ.0) THEN
        IRET = 9
        WRITE (MSGTXT,1105)
        GO TO 990
        END IF
C                                       Get calibrators
      IF (SINGLE) THEN
         VCALID(1) = 1
      ELSE
         CALL LPCSOU (GETFLX, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Only one calibrator for now.
      NVCAL = 1
C                                       Model polarizations
C                                       Create new file with room for
C                                       models.
      CALL COPY (256, CATBLK, SCRCAT)
      SCLREC = LREC
      SCRPRM = NRPARM
      COMPDT = CATBLK(KINAX).EQ.1
      DATDIV = .TRUE.
      IF (COMPDT) THEN
         CALL AXEFND (8, 'WEIGHT  ', SCRCAT(KIPCN), SCRHOL(KHPTP),
     *      WTLOC, IRET)
C                                       Must have this one
         IF ((IRET.NE.0) .OR. (WTLOC.LT.0)) THEN
            IRET = 5
            MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED
     *         DATA'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         IRET = 0
         END IF
      CALL UVPAD (NMODEL, VISDSK, VISCNO, VMCNO, JBUFSZ, BUFF1, BUFF2,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy relevant portion of IF
C                                       table.
      IIVER = 1
      OOVER = 1
      CALL CHNCOP (IIVER, OOVER, LUN1, LUN2, DISKIN, SCRVOL(VMCNO),
     *     CNOIN, SCRCNO(VMCNO), CATUV, CATBLK, BIF, EIF, FRQSEL,
     *     SFREQS, BUFF1, BUFF2, UBUFF, UBUFF(2049), IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Save header values.
      SAVNRP = NRPARM
      SAVLRC = LREC
      SAVCR0 = ICOR0
      SAVSTK = CATR(KRCIC+JLOCS)
      NRPARM = LREC
      LREC = SAVLRC + 3*NMODEL
      ICOR0 = ABS (ICOR0)
      CATR(KRCIC+JLOCS) = ABS (CATR(KRCIC+JLOCS))
      CATD(KDCRV+JLOCS) = ABS (CATD(KDCRV+JLOCS))
      CALL CATIO ('UPDT', SCRVOL(VMCNO), SCRCNO(VMCNO), CATBLK,
     *     'REST', BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1110) IRET
         GO TO 990
         END IF
C                                       Model computation
      CALL LPCFLX (IFNO, IRET)
      IF (IRET.NE.0) GO TO 999
C
      LTIMEP = -1.0E10
      LSOU = -10
C                                       Zero accumulations
      LIMIT = MXDAT
      IF (NVIS.LT.LIMIT) LIMIT = NVIS
      DO 120 LOOP = 1,LIMIT
         ANTS(1,LOOP) = 0
         ANTS(2,LOOP) = 0
         PARAN(1,LOOP) = 0.0
         PARAN(2,LOOP) = 0.0
         VWT(1,LOOP) = 0.0
         VWT(2,LOOP) = 0.0
         VWT(3,LOOP) = 0.0
         VWT(4,LOOP) = 0.0
         VOBS(1,1,LOOP) = 0.0
         VOBS(2,1,LOOP) = 0.0
         VOBS(1,2,LOOP) = 0.0
         VOBS(2,2,LOOP) = 0.0
         VOBS(1,3,LOOP) = 0.0
         VOBS(2,3,LOOP) = 0.0
         VOBS(1,4,LOOP) = 0.0
         VOBS(2,4,LOOP) = 0.0
         DO 110 IMODEL = 1,NMODEL
            VMOD(1,IMODEL,LOOP) = 0.0
            VMOD(2,IMODEL,LOOP) = 0.0
 110        CONTINUE
 120    CONTINUE
C                                       Initialize counters and pointers
      NUMBL = (NUMANT * (NUMANT-1)) / 2
      CALL FILL (NUMBL, 0, SUMCNT)
      CALL FILL (NUMBL, 0, SUMPNT)
      CSOUID = 0
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('SC', SCRVOL(VMCNO), SCRCNO(VMCNO), 1, IFILE, IRET)
      CALL ZOPEN (LUN1, IND, SCRVOL(VMCNO), IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
      ILENBU = 1
      CALL UVINIT ('READ', LUN1, IND, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IPTRI = KBIND
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1121) IRET
         GO TO 990
         END IF
C                                       Read data
      CURSOU = VCALID(1)
      IF (CURSOU.LE.0) CURSOU = 1
      NEXT = 1
      FIRST = 1
      CNTBL = 0
      TSCEND = -1.0E10
      LOPCNT = 0
      DO 200 LOOP = 1,NVIS
C                                       Read vis. record.
         CALL UVDISK ('READ', LUN1, IND, BUFF2, INIO, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1124) IRET
            GO TO 990
            END IF
         IPTRI = KBIND
         IOFF = IPTRI + SAVNRP - 1
         IF (INIO.LE.0) GO TO 210
         CTIME = BUFF2(IPTRI+ILOCT)
         IF (TSCEND.LT.-100.0) TSCEND = CTIME + SOLINT
C                                       Get source info.
         IF (ILOCSU.GE.0) CURSOU = BUFF2(IPTRI+ILOCSU) + 0.1
         IF (CURSOU.NE.LSOU) THEN
           CALL GETSOU (CURSOU, DISKIN, CNOIN, CATUV, LUN1, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Find cal. number.
            CSOUID = 0
            DO 140 I = 1,NVCAL
C                                       Use source ID if being fitted
               IF (CURSOU.EQ.VCALID(I)) CSOUID = I
 140           CONTINUE
            END IF
C                                       See if average finished
         IF ((CTIME.GT.TSCEND) .OR. (CURSOU.NE.LSOU)) THEN
            INDEX = FIRST
C                                       Average
            DO 150 IVIS = 1,CNTBL
               XNORM = 1.0
               IF (SUMCNT(IVIS).GT.0) XNORM = 1.0 / SUMCNT(IVIS)
               TIME(INDEX) = TIME(INDEX) * XNORM
               PARAN(1,INDEX) = PARAN(1,INDEX) * XNORM
               PARAN(2,INDEX) = PARAN(2,INDEX) * XNORM
               VOBS(1,1,INDEX) = VOBS(1,1,INDEX) * XNORM
               VOBS(2,1,INDEX) = VOBS(2,1,INDEX) * XNORM
               VOBS(1,2,INDEX) = VOBS(1,2,INDEX) * XNORM
               VOBS(2,2,INDEX) = VOBS(2,2,INDEX) * XNORM
               VOBS(1,3,INDEX) = VOBS(1,3,INDEX) * XNORM
               VOBS(2,3,INDEX) = VOBS(2,3,INDEX) * XNORM
               VOBS(1,4,INDEX) = VOBS(1,4,INDEX) * XNORM
               VOBS(2,4,INDEX) = VOBS(2,4,INDEX) * XNORM
               VWT(1,INDEX) = MAX (VWT(1,INDEX), 0.0)
               VWT(2,INDEX) = MAX (VWT(2,INDEX), 0.0)
               VWT(3,INDEX) = MAX (VWT(3,INDEX), 0.0)
               VWT(4,INDEX) = MAX (VWT(4,INDEX), 0.0)
               INDEX = INDEX + 1
 150           CONTINUE
            INDEX = FIRST
C                                       Average Model
            DO 160 IVIS = 1,CNTBL
               XNORM = 1.0
               IF (SUMCNT(IVIS).GT.0) XNORM = 1.0 / SUMCNT(IVIS)
               DO 155 IMODEL = 1,NMODEL
                  VMOD(1,IMODEL,INDEX) = VMOD(1,IMODEL,INDEX) * XNORM
                  VMOD(2,IMODEL,INDEX) = VMOD(2,IMODEL,INDEX) * XNORM
 155              CONTINUE
               INDEX = INDEX + 1
 160           CONTINUE
C                                       Reset for next average.
            CNTBL = 0
            FIRST = NEXT
            CALL FILL (NUMBL, 0, SUMCNT)
            CALL FILL (NUMBL, 0, SUMPNT)
            TSCEND = CTIME + SOLINT
            IF (NEXT.GT.MXDAT) GO TO 210
            END IF
         LSOU = CURSOU
         IF (ILOCB.GE.0) THEN
            IA1 = BUFF2(IPTRI+ILOCB) / 256.0 + 0.1
            IA2 = BUFF2(IPTRI+ILOCB) - IA1*256 + 0.1
         ELSE
            IA1 = BUFF2(IPTRI+ILOCA1) + 0.1
            IA2 = BUFF2(IPTRI+ILOCA2) + 0.1
            END IF
C                                       Get baseline pointer.
         BLNDX = ((IA1-1)*NUMANT) - (((IA1+1)*IA1)/2) + IA2
         IF (SUMPNT(BLNDX).LE.0) THEN
            IF (NEXT.GT.MXDAT) GO TO 210
            SUMPNT(BLNDX) = NEXT
            NEXT = NEXT + 1
            CNTBL = CNTBL + 1
            END IF
         LOPCNT = LOPCNT + 1
         INDEX = SUMPNT(BLNDX)
         GNDX = INDEX - FIRST + 1
         SUMCNT(GNDX) = SUMCNT(GNDX) + 1
         ANTS(1,INDEX) = IA1
         ANTS(2,INDEX) = IA2
         VSOU(INDEX) = CSOUID
C                                       Parallactic angle
         IF (CTIME.GT.LTIMEP) THEN
            CALL PARANG (CTIME, PANGLE)
            LTIMEP = CTIME
            END IF
         TIME(INDEX) = TIME(INDEX) + BUFF2(IPTRI+ILOCT)
         PARAN(1,INDEX) = PARAN(1,INDEX) + PANGLE(IA1)
         PARAN(2,INDEX) = PARAN(2,INDEX) + PANGLE(IA2)
C                                       Gather visibilities
      INCLUDE 'INCS:ZVND.INC'
         DO 170 LOOP2 = 1,8
            GNDX = GATNDX(LOOP2) + IOFF
            VOBSX(LOOP2,INDEX) = VOBSX(LOOP2,INDEX) + BUFF2(GNDX)
 170        CONTINUE
C                                       Weights
         VWT(1,INDEX) = VWT(1,INDEX) + BUFF2(IOFF+3)
         VWT(4,INDEX) = VWT(4,INDEX) + BUFF2(IOFF+6)
         VWT(2,INDEX) = VWT(2,INDEX) + BUFF2(IOFF+9)
         VWT(3,INDEX) = VWT(3,INDEX) + BUFF2(IOFF+12)
C                                       Gather models
      INCLUDE 'INCS:ZVND.INC'
         DO 200 IMODEL = 1,NMODEL
            GNDX = IOFF + 13 + (IMODEL-1) * 3
            VMOD(1,IMODEL,INDEX) = VMOD(1,IMODEL,INDEX) + BUFF2(GNDX)
            VMOD(2,IMODEL,INDEX) = VMOD(2,IMODEL,INDEX) + BUFF2(GNDX+1)
 200     CONTINUE
C                                       Finish last accumulation
 210  INDEX = FIRST
      DO 250 IVIS = 1,CNTBL
         XNORM = 1.0
         IF (SUMCNT(IVIS).GT.0) XNORM = 1.0 / SUMCNT(IVIS)
         TIME(INDEX) = TIME(INDEX) * XNORM
         PARAN(1,INDEX) = PARAN(1,INDEX) * XNORM
         PARAN(2,INDEX) = PARAN(2,INDEX) * XNORM
         VOBS(1,1,INDEX) = VOBS(1,1,INDEX) * XNORM
         VOBS(2,1,INDEX) = VOBS(2,1,INDEX) * XNORM
         VOBS(1,2,INDEX) = VOBS(1,2,INDEX) * XNORM
         VOBS(2,2,INDEX) = VOBS(2,2,INDEX) * XNORM
         VOBS(1,3,INDEX) = VOBS(1,3,INDEX) * XNORM
         VOBS(2,3,INDEX) = VOBS(2,3,INDEX) * XNORM
         VOBS(1,4,INDEX) = VOBS(1,4,INDEX) * XNORM
         VOBS(2,4,INDEX) = VOBS(2,4,INDEX) * XNORM
         VWT(1,INDEX) = MAX (VWT(1,INDEX), 0.0)
         VWT(2,INDEX) = MAX (VWT(2,INDEX), 0.0)
         VWT(3,INDEX) = MAX (VWT(3,INDEX), 0.0)
         VWT(4,INDEX) = MAX (VWT(4,INDEX), 0.0)
         INDEX = INDEX + 1
 250     CONTINUE
C                                       Average model
      INDEX = FIRST
      DO 260 IVIS = 1,CNTBL
         XNORM = 1.0
         IF (SUMCNT(IVIS).GT.0) XNORM = 1.0 / SUMCNT(IVIS)
         DO 255 IMODEL = 1,NMODEL
            VMOD(1,IMODEL,INDEX) = VMOD(1,IMODEL,INDEX) * XNORM
            VMOD(2,IMODEL,INDEX) = VMOD(2,IMODEL,INDEX) * XNORM
 255        CONTINUE
         INDEX = INDEX + 1
 260     CONTINUE
C                                       Number of observations.
      NUMDAT = NEXT - 1
      IF (LOPCNT.LT.NVIS) THEN
C                                       Couldn't fit some data:
         WRITE (MSGTXT,1250)
         CALL MSGWRT (6)
         WRITE (MSGTXT,1251) LOPCNT, NVIS
         CALL MSGWRT (6)
         WRITE (MSGTXT,1252)
         CALL MSGWRT (6)
         END IF
C                                       Force new scratch files next
C                                       pass.
      VISDSK = 0
      VISCNO = 0
      VMCNO = 0
C                                       Close scratch files
      IF (IRET.EQ.0) CALL ZCLOSE (LUN1, IND, IRET)
      IF (IRET.EQ.0) GO TO 999
         WRITE (MSGTXT,1300) IRET
         GO TO 990
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Averaging IFs ',I4, ' to ',I4)
 1001 FORMAT ('Processing IF number ',I4)
 1100 FORMAT ('LPCSEL: NO DATA SELECTED FOR IF', I4)
 1105 FORMAT ('NO CALIBRATORS SELECTED')
 1110 FORMAT ('LPCSEL: ERROR ',I5,' UPDATING SCRATCH FILE CATBLK')
 1120 FORMAT ('LPCSEL: ZOPEN ERROR ',I3,' OPENING SCRATCH FILE')
 1121 FORMAT ('LPCSEL: UVINIT ERROR ',I3,' INITIALIZING SCRATCH FILE')
 1124 FORMAT ('LPCSEL: UVDISK ERROR ',I3,' READING SCRATCH FILE')
 1250 FORMAT ('LPCSEL: WARNING: INTERNAL ARRAYS TOO SMALL, USING ONLY')
 1251 FORMAT ('LPCSEL: ',I9,' OF ',I9,' VISIBILITIES')
 1252 FORMAT ('LPCSEL: USE LONGER SOLUTION INTERVAL (SOLINT)')
 1300 FORMAT ('LPCSEL: ZCLOSE ERROR ',I3,' CLOSING SCRATCH FILE')
      END
      SUBROUTINE UVPAD (NMODEL, DISK, CNOSCR, ISCR2, JBUFSZ, BUFF1,
     *   BUFF2, IRET)
C-----------------------------------------------------------------------
C! Reformat UV data record, making space for total intensity models.
C# UV Modeling IO-appl
C   This software is the subject of a User agreement and is confidential
C   in nature. It shall not be sold or otherwise made available or
C   disclosed to third parties.
C-----------------------------------------------------------------------
C   UVPAD reformats the uv data in the input file by making space
C   for NMODEL model visibilities and zero filling it.  Model values
C   can then be computed and placed in this space for the feed
C   solution.  The values in /UVHDR/ common (filled by UVPGET)
C   are assumed valid for the input file.  If disk.LE.0 then CNOSCR
C   is assumed to be the /CFILES/ scratch file number.  Uses LUNs 24
C   and 25 which must not already be in use.
C      If the scratch file ISCR2 does not exist (denoted by ISCR2 .LE.
C   0) then it will be created and entered into /CFILES/ so that it can
C   be destroyed by DIE.
C   Inputs:
C      NMODEL   I      Number of total intensity models.
C      DISK     I      Disk number for catalogd input file. If.LE.0
C                      then the input file is a /CFILES/ scratch file.
C      CNOSCR   I      Catalog slot number for if catalogd file;
C                      /CFILES/ scratch file number if a scratch file,
C      ISCR2    I      /CFILES/ number of scratch file for output,
C                      If.LE.0 then the file will be created.
C      JBUFSZ   I      The size of BUFF1 and BUFF2 in bytes.
C   Inputs from common /UVHDR/:
C      LREC     I      Length of visibility record in R   words.
C      NRPARM   I      Number of random parameters (R   words)
C      NVIS     I      Number of visibilities
C   Output:
C      BUFF1    R(*)   Work buffers
C      BUFF2    R(*)   Work buffers
C      ISCR2    I      /CFILES/ number of output scratch file.
C      IRET     I      Return code, 0 => OK, otherwise failed.
C
C   Modified from UVDPAD. KJL 3/3/94.
C-----------------------------------------------------------------------
      INTEGER   NMODEL, DISK, CNOSCR, ISCR2, JBUFSZ, IRET
      REAL      BUFF1(*), BUFF2(*)
C
      CHARACTER NAME*48
      INTEGER   VOL, LUNI, LUNO, FINDI, FINDO, BINDI, BINDO, LENBI,
     *   LENBO, INIO, ONIO, LRECO, OCNT, ILOOP, NZERO, BO, VO, ISIZE,
     *   LRECUN, UNPARM, ILOCWT
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSCD.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNI, LUNO, BO, VO /24,25, 1,0/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Set input file name.
      IF (DISK.LE.0) THEN
         VOL = SCRVOL(CNOSCR)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCR), 1, NAME, IRET)
      ELSE
         VOL = DISK
         CALL ZPHFIL ('UV', VOL, CNOSCR, 1, NAME, IRET)
         END IF
C                                       Open input file.
      CALL ZOPEN (LUNI, FINDI, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'READ', IRET
         GO TO 990
         END IF
C                                       Setup to init I/O, determine
C                                       size.
      LENBI = 0
      CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC, LENBI, JBUFSZ,
     *   BUFF1, BO, BINDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'READ', IRET
         GO TO 990
         END IF
C                                       If compressed, what would
C                                       uncompressed size be?
      LRECUN = LREC
      UNPARM = NRPARM
      IF (COMPDT) THEN
         CALL AXEFND (8, 'WEIGHT  ', SCRCAT(KIPCN), SCRHOL(KHPTP),
     *      ILOCWT, IRET)
         IF ((IRET.NE.0) .OR. (ILOCWT.LT.0)) THEN
            IRET = 1
            MSGTXT = 'UVPAD: CANNOT FIND WEIGHT/SCALE FOR COMPRESSED
     *         DATA'
            GO TO 990
            END IF
         LRECUN = LREC - NRPARM
         LRECUN = LRECUN * 3
         LRECUN = LRECUN + NRPARM - 2
         UNPARM = NRPARM - 2
C                                       Update SCRHDR data
         SCLREC = LRECUN + 3*NMODEL
         SCRPRM = LRECUN
         SCRCAT(KINAX) = SCRCAT(KINAX) * 3
         END IF
C                                       Check if scratch file exists,
C                                       if not create it.
      IF (ISCR2.LE.0) THEN
C                                       Create scratch file.
C                                       Must be uncompressed
         CALL UVSIZE ((LRECUN + 3*NMODEL), NVIS, ISIZE)
         CALL SCREAT (ISIZE, BUFF2, IRET)
         ISCR2 = NSCR
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) WRITE (MSGTXT,1020)
            IF (IRET.GT.1) WRITE (MSGTXT,1021) IRET
            GO TO 990
            END IF
         END IF
C                                       Open output file.
      CALL ZPHFIL ('SC', SCRVOL(ISCR2), SCRCNO(ISCR2), 1, NAME, IRET)
      CALL ZOPEN (LUNO, FINDO, SCRVOL(ISCR2), NAME, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'WRIT', IRET
         GO TO 990
         END IF
C                                       Setup for init.
      LRECO = LRECUN + 3*NMODEL
      LENBO = 0
      CALL UVINIT ('WRIT', LUNO, FINDO, NVIS, VO, LRECO, LENBO, JBUFSZ,
     *   BUFF2, BO, BINDO, IRET)
      OCNT = 0
      ONIO = LENBO
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'WRIT', IRET
         GO TO 990
         END IF
C                                       Set up for reformatting.
      NZERO = 3 * NMODEL
C                                       Begin loop
 100     CALL UVDISK ('READ', LUNI, FINDI, BUFF1, INIO, BINDI, IRET)
         IF (INIO.LE.0) GO TO 250
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'READ', IRET
            GO TO 990
            END IF
C                                       Loop copying to output, zero
C                                       padding.
         DO 200 ILOOP = 1,INIO
            IF (COMPDT) THEN
               CALL RCOPY (UNPARM, BUFF1(BINDI), BUFF2(BINDO))
               CALL ZUVXPN ((LREC-NRPARM), BUFF1(BINDI+NRPARM),
     *            BUFF1(BINDI+ILOCWT), BUFF2(BINDO+UNPARM))
            ELSE
               CALL RCOPY (LREC, BUFF1(BINDI), BUFF2(BINDO))
               END IF
C                                       Zero fill upper portion.
            CALL RFILL (NZERO, 0.0, BUFF2(BINDO+LRECUN))
C                                       Update pointers.
            BINDI = BINDI + LREC
            BINDO = BINDO + LRECO
C                                       Check if time for output.
            OCNT = OCNT + 1
C                                       Write
            IF (OCNT.GE.ONIO) THEN
               ONIO = OCNT
               CALL UVDISK ('WRIT', LUNO, FINDO, BUFF2, ONIO, BINDO,
     *            IRET)
               OCNT = 0
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) 'WRIT', IRET
                  GO TO 990
                  END IF
               END IF
 200        CONTINUE
C                                       Loop back for more.
         GO TO 100
C                                       Done - flush output buffer.
 250  ONIO = -OCNT
      CALL UVDISK ('FLSH', LUNO, FINDO, BUFF2, ONIO, BINDO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) 'FLSH', IRET
         GO TO 990
         END IF
C                                       Close files.
      CALL ZCLOSE (LUNI, FINDI, IRET)
      CALL ZCLOSE (LUNO, FINDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVPAD: OPEN FOR ',A4,' ERROR ',I5)
 1010 FORMAT ('UVPAD: INIT FOR ',A4,' ERROR ',I5)
 1020 FORMAT ('UVPAD: TOO LITTLE DISK SPACE FOR SCRATCH FILE')
 1021 FORMAT ('UVPAD: ERROR ',I5,' CREATING SCRATCH FILE')
 1100 FORMAT ('UVPAD: ',A4,' ERROR ',I5)
      END
      SUBROUTINE LPCSOU (GETFLX, IRET)
C-----------------------------------------------------------------------
C   LPCSOU collects calibrator polarized flux density information.
C   Input:
C      GETFLX       L    If true fill VFLUX from SU table.
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
C   Output to Common:
C    NVCAL         I    Number of calibrator sources
C    VCALID(*)     I    Calibrator ID numbers
C    VFLUX(4,*,if) R    Polarized flux densities (I,Q,U,V) (Jy)
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C   Note: also uses buffer NXBUFF
C-----------------------------------------------------------------------
      LOGICAL   GETFLX
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   I, SUKOLS(MAXSUC), SUNUMV(MAXSUC), IDSOU, QUAL, SULUN,
     *   ISURNO, NUMSOU, LOOP, SUFQID
      LOGICAL   T, LTEMP
      DOUBLE PRECISION BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'LPCAL.INC'
      INCLUDE 'INCS:DSEL.INC'
      DOUBLE PRECISION LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      REAL     FLUX(4,MAXIF)
      DATA SULUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      NVCAL = 0
C                                       Open source (SU) table
      CALL SOUINI ('READ', NXBUFF, DISKIN, CNOIN, 1, CATUV,
     *   SULUN, NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Check FREQID compatibility.
      IF ((SUFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *   (SUFQID.NE.FREQID)) THEN
         WRITE (MSGTXT,1030)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1040) SUFQID
         CALL MSGWRT (4)
         WRITE (MSGTXT,1050) FREQID
         CALL MSGWRT (4)
         WRITE (MSGTXT,1060)
         CALL MSGWRT (4)
         IRET = 5
         GO TO 999
         END IF
C                                       Get number of sources
      NUMSOU = NXBUFF(5)
C                                       Read flux array
      DO 50 LOOP = 1,NUMSOU
         ISURNO = LOOP
         CALL TABSOU ('READ', NXBUFF, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *      PMRA, PMDEC, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Save calibrator fluxes
C                                       See if wanted
         IF (NSOUWD.LE.0) GO TO 30
            DO 20 I = 1,NSOUWD
               IF (IDSOU.EQ.SOUWAN(I)) GO TO 30
 20            CONTINUE
C                                       Not wanted
            GO TO 50
C                                       Wanted
 30      IF (NVCAL.LT.MAXCAL) THEN
            NVCAL = NVCAL + 1
            VCALID(NVCAL) = IDSOU
            LTEMP = T
            IF (GETFLX) THEN
               DO 35 I = 1,NUMIF
                  VFLUX(1,NVCAL,I) = FLUX(1,I)
                  VFLUX(2,NVCAL,I) = FLUX(2,I)
                  VFLUX(3,NVCAL,I) = FLUX(3,I)
                  VFLUX(4,NVCAL,I) = FLUX(4,I)
 35               CONTINUE
               END IF
         ELSE
C                                       Too many calibrators
            IRET = 0
            WRITE (MSGTXT,1035)
            CALL MSGWRT (6)
            GO TO 100
            END IF
 50      CONTINUE
C                                       Close table
 100  CALL TABIO ('CLOS', 0, ISURNO, BUFF1, NXBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1070) IRET
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LPCSOU: ERROR',I3,' OPENING SOURCE TABLE')
 1010 FORMAT ('LPCSOU: ERROR',I3,' READING SOURCE TABLE')
 1030 FORMAT ('WARNING - POTENTIALLY FATAL ERROR')
 1040 FORMAT ('   Your calibrators have their fluxes set for FQID ',I3)
 1050 FORMAT ('   You are using them to calibrate FQID ',I3)
 1060 FORMAT ('   Suggest you rerun SETJY with the correct FREQID')
 1035 FORMAT ('WARNING - TOO MANY CALIBRATORS, USING ONLY THE FIRST')
 1070 FORMAT ('LPCSOU: ERROR',I3,' CLOSING SOURCE TABLE')
      END
      SUBROUTINE LPCFLX (IFNO, IRET)
C-----------------------------------------------------------------------
C   LPCFLX includes the CLEAN model visibilities into the data.
C   The model visibilities are computed in the form of Stokes I
C   and placed in the file created by LPCSEL with space
C   for both observed and model visibilities. The desired models
C   must be images or CC tables with consecutive version numbers
C   starting at CCTVER. If no model is found an error is returned.
C   NOTE: Visibility models derived from images are incorrect
C         probably due to a bug in VISDFT. Use only CC models.
C   Inputs from commons:
C     VMDSK     I    Input file disk number (0=scratch)
C     VMCNO     I    Input file catalog number.
C     NAME2     C*12 CLEAN model name.
C     CLAS2     C*6  Clean model class
C     DISK2     I    CLEAN file disk number.
C     CCTVER    I    First CC table version number.
C     CLNMOD    L    true=>use CLEAN model, else used image
C   Output:
C     IRET      I    Return code, 0 => ok, otherwise not.
C-----------------------------------------------------------------------
      INTEGER   IFNO, IRET
C
      CHARACTER  CLASS*6
      INTEGER   MODEL, METHOD, ISTOKE, IFIELD, IMODEL, I4T, DISKO,
     *   ISCR, CHAN, NCHAN, SAVNCL, SAVCID, SAVNRP, VER, MODNUM,
     *   NITER, BITER
      LOGICAL   DOMSG, DOSUM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'LPCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA DOMSG, DOSUM /.FALSE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Save some values
      SAVNCL = NVCAL
      SAVCID = VCALID(1)
      SAVNRP = NRPARM
C                                       Set model and method
      MODEL = 0
      IF (CMOD.EQ.'COMP') MODEL = 1
      IF (CMOD.EQ.'IMAG') MODEL = 2
      IF (CMOD.EQ.'SUBI') MODEL = 3
      METHOD = 0
      IF (CMETH.EQ.'DFT ') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
C     IF (CLNMOD) THEN
C        MODEL = 1
C        METHOD = -1
C     ELSE
C        MODEL = 2
C        METHOD = 1
C        END IF
      DOPTMD = .FALSE.
C                                       ADD the models.
      FACGRD(1) = -1.0
      FACGRD(2) = 1.0
C                                       Use all CLEAN comps
      NONEG = .FALSE.
C                                       Loop over the fields.
      MODNUM = 0
      DO 140 IFIELD = 1,NFIELD
C                                       Get correct class.
         CLASS = CLAS2
         I4T = IFIELD - 1
         IF (IFIELD.NE.1) CALL ZEHEX (I4T, 2, CLASS(5:6))
         IF (.NOT.CLNMOD) NCOMP(IFIELD) = 1
C                                       Loop over the models.
         DO 120 IMODEL = 1,NCOMP(IFIELD)
C                                       One CC table at a time.
            ISTOKE = 1
            NITER = 0
            BITER = 1
            VER = CCTVER + IMODEL -1
            MODNUM = MODNUM + 1
C                                       Message about model origin
            IF (NUMHIS.GE.NMODEL) GO TO 110
               IF (CLNMOD) THEN
                  WRITE (MSGTXT,1040) MODNUM, NAME2, CLASS, SEQ2, VER
               ELSE
                  WRITE (MSGTXT,1042) MODNUM, NAME2, CLASS, SEQ2
                  END IF
               NUMHIS = NUMHIS + 1
               HISCRD(NUMHIS) = MSGTXT
               CALL MSGWRT (4)
 110           CONTINUE
C                                       Set up for model computation.
            CALL SETGDS (DISKIN, CNOIN, NAME2, CLASS, SEQ2, DISK2, 1,
     *         VER, NITER, BITER, MODEL, METHOD, BUFF1, BUFF2, ISTOKE,
     *         IRET)
            IF (IRET.NE.0 .OR. (CLNMOD .AND. MODEL.GT.1)) THEN
               IRET = 2
               GO TO 999
               END IF
            IF (MODEL.GT.0) THEN
               IF (MODEL.EQ.3) THEN
                  MSGTXT = 'Using sub-images for the source model'
               ELSE IF (MODEL.EQ.2) THEN
                  MSGTXT = 'Using images for the source model'
               ELSE
                  MSGTXT = 'Using Clean Component source model'
                  END IF
               CALL MSGWRT (3)
               CALL FACSET (DISKIN, CNOIN, IFNO, SOUWAN(1), MODEL,
     *            FACGRD(1), IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       Stokes I, RR, or LL required
            IF (ISTOKE.NE.1 .AND. ISTOKE.NE.-1 .AND. ISTOKE.NE.-2) THEN
               WRITE (MSGTXT, 1050)
               IRET = 9
               GO TO 990
               END IF
C                                       Compute model.
            DISKO = VMDSK
            ISCR = VMCNO
            CHAN = 1
            NCHAN = 1
            NUMIF = 1
            CALL UVMSUB (DISKO, ISCR, DISKO, ISCR, 0, MODEL, METHOD,
     *         CHAN, NCHAN, DOSUM, DOMSG, CATBLK, JBUFSZ, FRQSEL, BUFF1,
     *         BUFF2, UBUFF, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL UNSETG (BUFF2)
C                                       Update vis. pointers.
            NRPARM = NRPARM + 3
 120        CONTINUE
 140     CONTINUE
      NRPARM = SAVNRP
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('Sub-model ',I3,':   ',A12,' . ',A6,' . ',I3,
     *   '    CCVER = ',I3)
 1042 FORMAT ('Sub-model ',I3,':   ',A12,' . ',A6,' . ',I3,
     *   '    (image used)')
 1050 FORMAT ('INCORRECT CLEAN MODEL STOKES, SHOULD BE I, RR, OR LL')
      END
      SUBROUTINE LPCHIS
C-----------------------------------------------------------------------
C   LPCHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12, HILINE*72, LABEL*8
      INTEGER   LUN, IERR, I, ITIME(3), DATE(3)
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'LPCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Open old history
      CALL HIOPEN (LUN, DISKIN, CNOIN, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (ITIME)
      CALL TIMDAT (ITIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       calibration history
      NSOUWD = MIN (1, NSOUWD)
      CALL CALHIS (LUN, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Add selection/calibration
C                                       criteria:
C                                       Write model info
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,3020) TSKNAM
         DO 100 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
 100        CONTINUE
         END IF
C                                       Solution interval
      WRITE (HILINE,3022) TSKNAM, XSOLIN
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Close HI file
 190   CALL HICLOS (LUN, T, BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 3020 FORMAT (A6,' /')
 3022 FORMAT (A6,' SOLINT =',F10.3,' /Solution interval (min)')
      END
      SUBROUTINE PUTSOU (NVCAL, VFLUX, VCALID, MXNCAL,
     *   BIF, EIF, DISK, CNO, CATBLK, LUN, FREQID, IERR)
C-----------------------------------------------------------------------
C   Routine to enter calibrator flux polarization flux densities into
C   the SU table.
C   Inputs:
C    NVCAL         I    Number of calibrator sources.
C    VFLUX(4,*,if) R    Flux densities I, Q, U, V (Jy)
C    VCALID(*)     I    Source ID numbers
C    MXNCAL        I    Dimension of VFLUX and VCALID
C    BIF           I    First IF number
C    EIF           I    Highest IF number
C    DISK          I    Disk number for NX and SN tables.
C    CNO           I    Catalog slot number
C    CATBLK(256)   I    Catalog header
C    LUN           I    LUN to use. (e.g. 25)
C    FREQID        I    FREQID info being written for
C   Output:
C    IERR          I    Return code. 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   MXNCAL
      INTEGER   NVCAL, VCALID(MXNCAL), BIF, EIF, DISK, CNO, CATBLK(*),
     *    LUN, FREQID, IERR, MSGSAV
      INCLUDE 'INCS:PUVD.INC'
      REAL      VFLUX (4,MXNCAL,*)
      INTEGER   BUFFER(512), VER, KOLS(MAXSUC), NUMV(MAXSUC),
     *   NUMIF, JERR, I, ICALID, IBIF, IEIF
      INTEGER   IRNO, NUMREC, LOOP
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initialize
      JERR = 0
      IERR = 0
C                                       See if any sources
      IF (NVCAL.LE.0) GO TO 999
C                                       Find Source Info
C                                       Open SU table
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL SOUINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   NUMIF, VELTYP, VELDEF, SUFQID, IRNO, KOLS, NUMV, JERR)
      MSGSUP = MSGSAV
      IF (JERR.NE.0) GO TO 999
C                                       Check IF limits
      IBIF = BIF
      IEIF = EIF
      IF (IBIF.LE.0) IBIF = 1
      IF (IBIF.GT.NUMIF) IBIF = NUMIF
      IF (IEIF.LE.IBIF) IEIF = IBIF
      IF (IEIF.GT.NUMIF) IEIF = NUMIF
C                                       Get number of records
      NUMREC = BUFFER(5)
C                                       Close and reopen write.
      CALL TABIO ('CLOS', 0, IRNO, BUFFER, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1090) IERR, 'SU'
         GO TO 990
 10   IF (NUMREC.LE.0) GO TO 999
      SUFQID = FREQID
      CALL SOUINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   NUMIF, VELTYP, VELDEF, SUFQID, IRNO, KOLS, NUMV, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find source
      DO 80 LOOP = 1,NUMREC
         IRNO = LOOP
         CALL TABSOU ('READ', BUFFER, IRNO, KOLS, NUMV, IDSOUR, SNAME,
     *      QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH,
     *      RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA, PMDEC,
     *      IERR)
         IF (IERR.EQ.0) GO TO 30
            WRITE (MSGTXT,1000) IERR, 'READ'
            GO TO 990
C                                       See if wanted
 30      DO 40 I = 1,NVCAL
            ICALID = I
            IF (IDSOUR.EQ.VCALID(I)) GO TO 50
 40         CONTINUE
C                                       Not wanted
         GO TO 80
C                                       Save flux densities
 50      DO 60 I = IBIF,IEIF
            FLUX(1,I) = VFLUX(1,ICALID,I)
            FLUX(2,I) = VFLUX(2,ICALID,I)
            FLUX(3,I) = VFLUX(3,ICALID,I)
            FLUX(4,I) = VFLUX(4,ICALID,I)
 60         CONTINUE
C                                       Rewrite record.
         IRNO = LOOP
         CALL TABSOU ('WRIT', BUFFER, IRNO, KOLS, NUMV, IDSOUR, SNAME,
     *      QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH,
     *      RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA, PMDEC,
     *      IERR)
         IF (IERR.EQ.0) GO TO 80
            WRITE (MSGTXT,1000) IERR, 'WRIT'
            GO TO 990
 80      CONTINUE
      CALL TABIO ('CLOS', 0, IRNO, BUFFER, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1090) IERR, 'SU'
         GO TO 990
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PUTSOU: TABSOU ERROR ',I3,1X,A4,'ING SU TABLE')
 1090 FORMAT ('PUTSOU: TABIO ERROR ',I3,' CLOSING ',A2,' TABLE')
      END
      SUBROUTINE PUTANT (DISK, CNO, INVER, OUTVER, BIF, EIF, NUMIF,
     *   SOLTYP, CATBLK, BUFF1, BUFF2, FREQID, IERR)
C-----------------------------------------------------------------------
C   PUTANT reads an antennas (AN) extension file and copies to an
C   output file adding antenna feed parameters.
C   Inputs:
C      DISK      I      Volume number
C      CNO       I      Catalog slot number
C      INVER     I      Input version number
C      OUTVER    I      Output version number
C      BIF       I      First IF number
C      EIF       I      Highest IF number
C      NUMIF     I      Number of IFs
C      SOLTYP    I      Feed solution type.
C                       1 = linear approximation
C                       2 = orientation, ellipticity
C                       3 = Lin. approx for X-Y feeds
C                       4 = VLBI linear approx.
C      CATBLK(*) I      Catalog header block
C      FREQID    I      FQ ID for which polzn parms being
C                       calculated
C   Input from COMMON (DANS.INC):
C      STNEPL(2,*)R    Feed real/elipticity (poln, IF)
C      STNORI(2,*)R    Feed imag/orientation (poln, IF)
C   Output:
C      BUFF1(*)  I      I/O Buffer
C      BUFF2(*)  I      I/O Buffer
C      IERR      I      Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CHPOLT*8, CHSOL(4)*8
      INTEGER   DISK, CNO, CATBLK(256), INVER, OUTVER, BUFF1(*),
     *   BUFF2(*), IERR,  BIF, EIF, NUMIF, SOLTYP, FREQID,
     *   A2NUMV(MAXANC), A2KOLS(MAXANC), IIF, LUN1, LUN2, IANT, INDEX,
     *   LOCS, KEYTYP, IBIF, IEIF, NUMREC
      HOLLERITH HSOLTY(2)
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CHSOL /'APPROX  ', 'ORI-ELP ','X-Y LIN ', 'VLBI'/
      DATA CHPOLT /'POLTYPE '/
      DATA LUN1, LUN2 /27,28/
C-----------------------------------------------------------------------
C                                      Open AN extension file.
      CALL ANTINI ('READ', BUFF1, DISK, CNO, INVER, CATBLK, LUN1,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ'
         GO TO 990
         END IF
C                                       Check FREQID compatibility.
      IF ((ANFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *   (ANFQID.NE.FREQID)) THEN
         WRITE (MSGTXT,1030)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1040)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1050) ANFQID
         CALL MSGWRT (4)
         WRITE (MSGTXT,1060) FREQID
         CALL MSGWRT (4)
         IERR = 5
         GO TO 999
         END IF
C
      NUMREC = BUFF1(5)
C                                       Check IF limits
      IBIF = BIF
      IEIF = EIF
      IF (IBIF.LE.0) IBIF = 1
      IF (IBIF.GT.NUMIF) IBIF = NUMIF
      IF (IEIF.LE.IBIF) IEIF = IBIF
      IF (IEIF.GT.NUMIF) IEIF = NUMIF
C                                       Open output table
      NOPCAL = 2
      ANTNIF = NUMIF
      ANFQID = FREQID
      CALL ANTINI ('WRIT', BUFF2, DISK, CNO, OUTVER, CATBLK, LUN2,
     *   IANRNO, A2KOLS, A2NUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRIT'
         GO TO 990
         END IF
C                                       Read AN records
      DO 200 IANT = 1,NUMREC
         IANRNO = IANT
         CALL TABAN ('READ', BUFF1, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1100) IERR, 'READ'
            GO TO 990
            END IF
C                                       Feed polarizations
         INDEX = 2 * (IBIF-1) + 1
         DO 150 IIF = IBIF,IEIF
            POLCA(INDEX) = STNELP(1,IIF,NOSTA)
            POLCA(INDEX+1) = STNORI(1,IIF,NOSTA)
            POLCB(INDEX) = STNELP(2,IIF,NOSTA)
            POLCB(INDEX+1) = STNORI(2,IIF,NOSTA)
            INDEX = INDEX + 2
 150        CONTINUE
C                                       Write record
         IANRNO = IANT
         CALL TABAN ('WRIT', BUFF2, IANRNO, A2KOLS, A2NUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'WRIT'
            GO TO 990
            END IF
 200     CONTINUE
C                                       Add solution type keyword.
      LOCS = 1
      KEYTYP = 3
      CALL CHR2H (8, CHSOL(SOLTYP), 1, HSOLTY)
      CALL TABKEY ('WRIT', CHPOLT, 1, BUFF2, LOCS, HSOLTY,
     *   KEYTYP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) IERR
         GO TO 990
         END IF
C                                       Close AN extension files
      CALL TABIO ('CLOS', 1, IANRNO, BUFF1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1210) IERR
         GO TO 990
         END IF
      CALL TABIO ('CLOS', 1, IANRNO, BUFF2, BUFF2, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1210) IERR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PUTANT: ERROR',I3,' OPEN-FOR-',A4,'ING AN FILE')
 1030 FORMAT ('WARNING:')
 1040 FORMAT ('   The polarization information in your AN table')
 1050 FORMAT ('   was set with FREQID ',I3,' this is being')
 1060 FORMAT ('   overwritten with parameters derived from FREQID ',I3)
 1100 FORMAT ('PUTANT: ERROR',I3,1X,A4,'ING AN FILE')
 1200 FORMAT ('PUTANT: ERROR',I3,' ADDING SOLUTION TYPE KEYWORD')
 1210 FORMAT ('PUTANT: ERROR',I3,' CLOSING AN FILE')
      END
      SUBROUTINE LPCCOP (DISK, CNOSCR, BUFFER, BUFSZ, IRET)
C-----------------------------------------------------------------------
C   Routine to copy selected data from one data file to another
C   optionally applying calibration and editing information.
C   The input file should have been opened with UVGET.
C   Both files will be closed on return from LPCCOP.
C     Note: UVGET returns the information necessary to catalog the
C   output file.  The output file will be compressed if necessary at
C   completion of LPCCOP.
C
C   Inputs:
C      DISK     I       Disk number for catalogd output file.
C                       If.LE.0 then the output file is a /CFILES/
C                       scratch file.
C      BUFFER   R(*)    Work buffer for writing.
C      BUFSZ    I       Size of BUFFER in bytes.
C
C   Input via common:
C      LREC     I       (/UVHDR/) length of vis. record in R words.
C      NRPARM   I       (/UVHDR/) number of R random parameters.
C
C   In/out:
C      CNOSCR   I       Catalog slot number for if cataloged file;
C                       /CFILES/ scratch file number if a scratch file,
C                       IF DISK=CNOSCR=0 then the scratch is created.
C                       On output = Scratch file number if created.
C   In/out via common:
C      CATBLK   I(256)  Catalog header block from UVGET
C                       on output with actual no. records
C      NVIS     I       (/UVHDR/) Number of vis. records.
C
C   Output:
C      IRET     I       Error code: 0 => OK,
C                          > 0 => failed, abort process.
C   Usage notes:
C   (1) UVGET with OPCODE='INIT' MUST be called before LPCCOP to setup
C       for calibration, editing and data translation.  If an output
C       cataloged file is to be created this should be done after the
C       call to UVGET.
C   (2) Uses AIPS LUN 24
C-----------------------------------------------------------------------
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER NAME*48
      INTEGER   DISK, CNOSCR, IRET, VOL, LUN, FIND, BIND, LENBU, NIO,
     *   BUFSZ, CNO, BO, VO, I, XCOUNT, ISIZE, LRECO, JERR, TNIF, TNF,
     *   TNS
      REAL      BUFFER(*), TBUFF(UVBFSS)
      PARAMETER (LUN = 24)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'LPCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA BO, VO /1,0/
C-----------------------------------------------------------------------
      IRET = 0
      LENBU = 1
C                                       Fix up for frequency and IF
C                                       averaging.
      LRECO = NRPARM + 4 * 3
      CATBLK(KINAX+JLOCF) = 1
      CATBLK(KINAX+JLOCIF) = 1
C                                       Create output file if necessary
      IF ((DISK.EQ.0) .AND. (CNOSCR.EQ.0)) THEN
C                                       Determine size.
         CALL UVSIZE (LREC, NVIS, ISIZE)
C                                       Create scratch file.
         CALL SCREAT (ISIZE, BUFFER, IRET)
         CNOSCR = NSCR
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) WRITE (MSGTXT,1000)
            IF (IRET.GT.1) WRITE (MSGTXT,1001) IRET
            GO TO 990
            END IF
C                                       Update CATBLK.
         CALL CATIO ('UPDT', SCRVOL(CNOSCR), SCRCNO(CNOSCR), CATBLK,
     *         'REST', BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1002) IRET
            END IF
         END IF
C                                       Set output file name.
      VOL = DISK
      IF (DISK.LE.0) VOL = SCRVOL(CNOSCR)
      IF (DISK.GT.0) CALL ZPHFIL ('UV', VOL, CNOSCR, 1, NAME, IRET)
      IF (DISK.LE.0) CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCR), 1, NAME,
     *   IRET)
      CNO = CNOSCR
      IF (DISK.LE.0) CNO = SCRCNO(CNOSCR)
C                                       Open output file.
      CALL ZOPEN (LUN, FIND, VOL, NAME, .TRUE., .FALSE., .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1003) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      CALL UVINIT ('WRIT', LUN, FIND, NVIS, VO, LRECO, LENBU, BUFSZ,
     *    BUFFER, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         GO TO 990
         END IF
C                                       Copy file
      TNIF = EIF - BIF + 1
      TNF = ECHAN - BCHAN + 1
      TNS = CATBLK(KINAX+JLOCS)
      DO 100 I = 1,NVIS
         XCOUNT = I
C                                       Read old.
         CALL UVGET ('READ', BUFFER(BIND), TBUFF, IRET)
         IF (IRET.LT.0) GO TO 110
         IF (IRET.NE.0) GO TO 999
C                                       Write new
         NIO = 1
C                                       Average in Frequency and IF
         CALL LPCAVG (TNIF, TNF, TNS, TBUFF, BUFFER(BIND+NRPARM))
         CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1008) IRET
            GO TO 990
            END IF
 100     CONTINUE
C                                       Check if last call to UVGET
C                                       returned valid data.
 110  IF (IRET.LT.0) XCOUNT = XCOUNT - 1
C                                       Flush output
      NIO = 0
      CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1008) IRET
         GO TO 990
         END IF

C                                       Close input
      CALL UVGET ('CLOS', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, VOL, CNO, LUN, CATBLK, IRET)
C                                      Put vis. count in CATBLK
      CATBLK(KIGCN) = NVIS
C                                       Update CATBLK.
      CALL CATIO ('UPDT', VOL, CNO, CATBLK, 'REST', BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1002) IRET
         END IF
      CALL UVPGET (JERR)
C                                       Close output
      CALL ZCLOSE (LUN, FIND, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LPCCOP: TOO LITTLE DISK SPACE FOR SCRATCH FILE')
 1001 FORMAT ('LPCCOP: ERROR ',I5,' CREATING SCRATCH FILE')
 1002 FORMAT ('LPCCOP: ERROR ',I5,' UPDATING SCRATCH FILE CATBLK')
 1003 FORMAT ('LPCCOP: ERROR ',I5,' OPENING OUTPUT FILE')
 1005 FORMAT ('LPCCOP: ERROR ',I5,' INIT. OUTPUT FILE')
 1008 FORMAT ('LPCCOP: ERROR ',I5,' WRITING OUTPUT FILE')
      END
      SUBROUTINE LPCAVG (TNIF, TNF, TNS, INBUF, OUTBUF)
C-----------------------------------------------------------------------
C   Routine to average a visibility in frequency and IF.
C   Inputs:
C      TNIF     I       Number of IFs to average
C      TNF      I       Number of frequencies to average
C      TNS      I       Number of stokes
C      INBUF    R(3,*)  Input visibility (re, im, wt)
C   Inputs from common (DUVH.INC)
C      JLOCIF   I       IF pointer
C      JLOCF    I       frequency pointer
C      JLOCS    I       Stokes pointer
C      INCIF    I       IF increment
C      INCF     I       frequency increment
C      INCS     I       Stokes increment
C   Outputs:
C      OUTBUF   R(3,4)  Output visibility
C-----------------------------------------------------------------------
      INTEGER   TNIF, TNF, TNS
      REAL      INBUF(*), OUTBUF(12)
C
      INTEGER   LOOPS, LOOPIF, LOOPF, INDEX, OUTDEX
      REAL      SUMWT, SUMRE, SUMIM, WT, XNORM
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Loop over Stokes'
      DO 40 LOOPS = 1,TNS
         OUTDEX = (LOOPS-1) * 3 + 1
         SUMWT = 0.0
         SUMRE = 0.0
         SUMIM = 0.0
C                                       Sum over freq and IF
         DO 30 LOOPIF = 1,TNIF
            INDEX = 1 + (LOOPS-1)*INCS + (LOOPIF-1)*INCIF
            DO 20 LOOPF = 1,TNF
               WT = MAX (0.0, INBUF(INDEX+2))
               SUMRE = SUMRE + INBUF(INDEX)*WT
               SUMIM = SUMIM + INBUF(INDEX+1)*WT
               SUMWT = SUMWT + WT
               INDEX = INDEX + INCF
 20            CONTINUE
 30         CONTINUE
         XNORM = 0.0
         IF (SUMWT.GT.1.0E-10) XNORM = 1.0 / SUMWT
         OUTBUF(OUTDEX)   = SUMRE * XNORM
         OUTBUF(OUTDEX+1) = SUMIM * XNORM
         OUTBUF(OUTDEX+2) = SUMWT
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE DPCALC (NOBS, VOBS, WT, CHI, IJS, NANT, D, NMODEL,
     *   VMOD, PRTLV, IER)
C-----------------------------------------------------------------------
C   Subroutine to determine instrumental and source polarizations.
C   A total intensity model for the calibrator must be provided
C   in the form of components for which the linear polarization
C   structure is assumed to be a scaled version of the total
C   intensity structure. Solves for complex leakage factors for
C   each station and the polarizations of each submodel.
C   Circular polarization is assumed to be negligible.
C   Only one calibrator source can be used.
C
C   Fits the model:
C
C     RL       = Pn*In        +  DRa * exp(i*2*chia) * LL
C                +         conj(DLb) * exp(i*2*chib) * RR
C     conj(LR) = Pn*conj(In)  +  DRb * exp(i*2*chib) * conj(LL)
C                +         conj(DLa) * exp(i*2*chia) * conj(RR)
C
C     where In is the flux of submodel n, Pn its fractional
C     linear polarization (Qn+iUn)/In, chia, chib are the parallactic
C     angles of antennas a and b, RR, RL, LR, LL are the observed
C     Stokes, and DLx and DRx are the instrumental parameters.
C
C   A standard complex linear least squares solution of the normal
C   equation is done by Cholesky decomposition using LAPACK routines.
C
C   Inputs:
C     NOBS       I    The number of observations, an observation
C                     consists of four visibility measuments on one
C                     baseline (RR, RL, LR, LL if using circular feeds).
C     VOBS(4,*)  CPLX Observed visibilities (RR, RL, LR, LL)
C     WT(4,*)    R    Weights of the visibilities
C     CHI(2,*)   R    Parallactic angles of the feeds for each antenna
C                     used for the observed visibilities.  For
C                     equatorial mounts use 0.
C     IJS(2,*)   I    Antenna numbers of the observations.  IJS(1,n)
C                     should be the lower number, IJS(2,n) the upper.
C     NANT       I    Number of antennas, actually the antenna number
C                     of the highest numbered antenna.
C     NMODEL     I    Number of submodels to fit.
C     VMOD(MAXMOD,*)
C                CPLX Model polarizations per observation (I,Q,U,V).
C     PRTLV      I    If .ge. 0 print fit results,
C                     if .ge. 1 print residuals,
C    Output:
C     D(2,*)     CPLX Feed parameters, 1 per feed (complex),
C                     first index: 1=>R, 2=>L.
C     IER        I    Return error code, 0=>OK, else failed.
C                     9 = inadequate total intensity model
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INTEGER   NCMAX, NVMAX, MAXMOD
C                                       Number of calibrators allowed:
      PARAMETER (NCMAX=1)
C                                       Number of models allowed:
      PARAMETER (MAXMOD=10)
      PARAMETER (NVMAX=2*MAXANT+MAXMOD)
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   NOBS, IER, NANT, NMODEL
      INTEGER   I, I1, I2, IND(NVMAX), J, K, L, KNVAR, KOBS, IMODEL,
     *   IJS(2,NOBS), LDA, NVAR, INFO, PRTLV, ANT, TNANT, CHACNT(MAXANT)
      REAL   CHI(2,NOBS), AMP, PHASE, IWT, AVG, W, WT(4,NOBS), RR, RI,
     *   RRTSUM, LLTSUM, RRTWT, LLTWT, CHTSUM, CHTWT, CH0SUM, CH0WT,
     *   PHASUM(MAXANT), PHAWT(MAXANT), CHASUM(MAXANT), CHAWT(MAXANT),
     *   RMS1, RMS2, PARNGE(2,MAXANT), DOWN, UP, AVGPAR
      COMPLEX C(NVMAX), D(2,MAXANT), VOBS(4,NOBS), VMOD(MAXMOD,NOBS),
     *   P(MAXMOD), ZZA, ZZB, MODVIS, RESID, T1, T2
      COMPLEX AHA(NVMAX,NVMAX), AHB(NVMAX)
C-----------------------------------------------------------------------
C                                       Check that necessary calibrator
C                                       model values are sent
      IF (NMODEL.GE.1) THEN
         DO 10 I = 1,NOBS
C                                       Check if total intensity model
C                                       given.
            IF (ABS (VMOD(1,I)).LE.1.0E-10) THEN
               IER = 9
               MSGTXT = 'TOTAL INTENSITY MODEL NOT PROVIDED'
               CALL MSGWRT (8)
               GO TO 999
               END IF
 10         CONTINUE
         END IF
C                                       Set constants
      IER = 0
      LDA = NVMAX
      NVAR = 2 * NANT + NMODEL
C                                       Set up the normal equations
C                                       (only the upper triangular
C                                       portion of AHA is filled in,
C                                       because the matrix is
C                                       conjugate-symmetric):
      DO 20 I = 1,NVAR
         AHB(I) = (0.0, 0.0)
         DO 15 J = I,NVAR
            AHA(I,J) = (0.0, 0.0)
 15         CONTINUE
 20      CONTINUE
      KOBS = 0
C                                       Matrix indices
      DO 30 IMODEL = 1,NMODEL
         IND(IMODEL+2) = 2 * NANT + IMODEL
 30      CONTINUE
C                                       Initialize par. angle ranges
      DO 35 ANT = 1,NANT
         PARNGE(1,ANT) = 999.0
 35      CONTINUE
C                                       Fill matrices
      DO 100 K = 1,NOBS
C                                       1st antenna number (A)
         I = IJS(1,K)
C                                       2nd antenna number (B)
         J = IJS(2,K)
C                                       Complex numbers with phase equal
C                                       to twice the parallactic
C                                       angles.
         ZZA = CMPLX (COS (2.0*(CHI(1,K))), SIN (2.0*(CHI(1,K))))
         ZZB = CMPLX (COS (2.0*(CHI(2,K))), SIN (2.0*(CHI(2,K))))
C                                       Total intensity
         IF ((WT(1,K).GT.0.0) .OR. (WT(4,K).GT.0.0)) THEN
            IF (WT(1,K).GT.0.0) IWT = ABS (VOBS(1,K))
            IF (WT(4,K).GT.0.0) IWT = ABS (VOBS(4,K))
         ELSE
            GO TO 100
            END IF
C                                       Update par. angle ranges
         DO 40 L = 1,2
            ANT = IJS(L,K)
            IF (PARNGE(1,ANT).EQ.999.0) THEN
               PARNGE(1,ANT) = CHI(L,K)
               PARNGE(2,ANT) = CHI(L,K)
            ELSE
               DOWN = PARNGE(1,ANT) - CHI(L,K)
               IF (DOWN.LT.PI) DOWN = DOWN + TWOPI
               IF (DOWN.GT.PI) DOWN = DOWN - TWOPI
               UP = CHI(L,K) - PARNGE(2,ANT)
               IF (UP.LT.PI) UP = UP + TWOPI
               IF (UP.GT.PI) UP = UP - TWOPI
               IF ((DOWN.GT.0.0) .AND. (DOWN.LE.ABS(UP)))
     *            PARNGE(1,ANT) = CHI(L,K)
               IF ((UP.GT.0.0) .AND. (UP.LE.ABS(DOWN)))
     *            PARNGE(2,ANT) = CHI(L,K)
               END IF
 40         CONTINUE
C                                       Contribution from RL
         IF (WT(2,K).GT.0.) THEN
            KOBS = KOBS+1
            W = WT(2,K) * IWT
C                                       Matrix indices
            IND(1) = I
            IND(2) = J + NANT
C                                       Derivatives of obs wrt
C                                       parameters:
C                                       C(1) = d(RL)/d(DRa)
C                                       C(2) = d(RL)/d(DLb*)
C                                       C(n) = d(RL)/d(P(n-2)), n>2
            IF (WT(4,K).GT.0) THEN
               C(1) = ZZA * VOBS(4,K)
            ELSE
               C(1) = ZZA * VOBS(1,K)
               END IF
            IF (WT(1,K).GT.0) THEN
               C(2) = ZZB * VOBS(1,K)
            ELSE
               C(2) = ZZB * VOBS(4,K)
               END IF
            DO 45 IMODEL=1,NMODEL
               C(IMODEL+2) = VMOD(IMODEL,K)
 45            CONTINUE
            DO 60 I1 = 1,2+NMODEL
               DO 50 I2 = I1,2+NMODEL
                  AHA(IND(I1),IND(I2)) = AHA(IND(I1),IND(I2))
     *               + W * CONJG (C(I1)) * C(I2)
 50               CONTINUE
               AHB(IND(I1)) = AHB(IND(I1))
     *            + W * CONJG (C(I1)) * VOBS(2,K)
 60            CONTINUE
            END IF
C                                       Contribution from LR
         IF (WT(3,K).GT.0.) THEN
            KOBS = KOBS + 1
            W = WT(3,K) * IWT
C                                       Matrix indices
            IND(1) = J
            IND(2) = I + NANT
C                                       Derivatives of obs wrt
C                                       parameters:
C                                       C(1) = d(LR*)/d(DRb)
C                                       C(2) = d(LR*)/d(DLa*)
C                                       C(n) = d(LR*)/d(P(n-2)), n>2
            IF (WT(4,K).GT.0) THEN
               C(1) = ZZB * CONJG (VOBS(4,K))
            ELSE
               C(1) = ZZB * CONJG (VOBS(1,K))
               END IF
            IF (WT(1,K).GT.0) THEN
               C(2) = ZZA * CONJG (VOBS(1,K))
            ELSE
               C(2) = ZZA * CONJG (VOBS(4,K))
               END IF
            DO 70 IMODEL = 1,NMODEL
               C(IMODEL+2) = CONJG (VMOD(IMODEL,K))
 70            CONTINUE
C
            DO 90 I1 = 1,2+NMODEL
               DO 80 I2 = I1,2+NMODEL
                  AHA(IND(I1),IND(I2)) = AHA(IND(I1),IND(I2))
     *               + W * CONJG (C(I1)) * C(I2)
 80               CONTINUE
               AHB(IND(I1)) = AHB(IND(I1))
     *            + W * CONJG (C(I1)) * CONJG (VOBS(3,K))
 90            CONTINUE
            END IF
 100     CONTINUE
C                                       Find the average size of the
C                                       nonzero diagonal elements of
C                                       AHA:
      K = 0
      AVG = 0.0
      DO 110 I = 1,NVAR
         IF (REAL (AHA(I,I)).GT.0.0) THEN
            K = K + 1
            AVG = AVG + REAL (AHA(I,I))
            END IF
 110     CONTINUE
      IF (K.GT.0) AVG = AVG / K
      IF (K.EQ.0) AVG = 1.0
C                                       For any missing antennas or
C                                       missing calibration sources,
C                                       set the corresponding diagonal
C                                       elements of the normal equations
C                                       matrix to the average of the
C                                       nonzero diagonal elements:
      KNVAR = NVAR
      DO 120 I = 1,NVAR
         IF (ABS (AHA(I,I)).EQ.0.0) THEN
            AHA(I,I) = CMPLX (AVG, 0.0)
            KNVAR = KNVAR - 1
            END IF
 120     CONTINUE

C                                       Solve the normal equations
C                                       using Cholesky decomposition
C                                       and LAPACK routines.
C                                       Cholesky decomposition
C                                       overwriting AHA with U.
      CALL CPOTRF ('U', NVAR, AHA, LDA, INFO)
C                                       Solving the linear equations.
C                                       AHB is owerwritten by the
C                                       solution vector.
      CALL CPOTRS ('U', NVAR, 1, AHA, LDA, AHB, LDA, INFO)
C                                       Save the solution
C                                       Leakage factors
      DO 220 I = 1,NANT
         D(1,I) = AHB(I)
         D(2,I) = CONJG (AHB(NANT+I))
 220     CONTINUE
C                                       Polarization solution
      DO 240 IMODEL = 1,NMODEL
         P(IMODEL) = AHB(2*NANT+IMODEL)
 240     CONTINUE
C
      IF (PRTLV.LT.0) GO TO 999
C                                       Now, print the results.
      DO 320 I = 1,NANT
         WRITE (MSGTXT,1240) I
         CALL MSGWRT (4)
         AMP = ABS (D(1,I))
         IF (AMP.EQ.0.0) THEN
            PHASE = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(1,I)), REAL (D(1,I))) * RAD2DG
            END IF
         WRITE (MSGTXT,1250) REAL (D(1,I)), AIMAG (D(1,I)), AMP, PHASE
         CALL MSGWRT (4)
C                                       Other hand poln.
         AMP = ABS (D(2,I))
         IF (AMP.EQ.0.0) THEN
            PHASE = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(2,I)), REAL (D(2,I))) * RAD2DG
            END IF
         WRITE (MSGTXT,1252) REAL (D(2,I)), AIMAG (D(2,I)), AMP, PHASE
         CALL MSGWRT (4)
 320     CONTINUE
C                                       Polarization sol.
      DO 340 IMODEL = 1,NMODEL
         WRITE (MSGTXT,1320) IMODEL
         CALL MSGWRT (4)
         WRITE (MSGTXT,1330) REAL (P(IMODEL)), AIMAG (P(IMODEL))
         CALL MSGWRT (4)
         AMP = ABS (P(IMODEL))
         IF (AMP.GT.0.0) THEN
            PHASE = 0.5 * ATAN2 (AIMAG (P(IMODEL)),
     *         REAL (P(IMODEL))) * RAD2DG
         ELSE
            PHASE = 0.0
            END IF
         WRITE (MSGTXT,1332) AMP, PHASE
         CALL MSGWRT (4)
 340     CONTINUE
      IF (PRTLV.LT.1) GO TO 999
C                                       Compute residuals.
      RRTWT = 0.0
      RRTSUM = 0.0
      LLTWT = 0.0
      LLTSUM = 0.0
      CHTWT = 0.0
      CHTSUM = 0.0
      CH0WT = 0.0
      CH0SUM = 0.0
      DO 520 I=1,MAXANT
         PHAWT(I) = 0.0
         PHASUM(I) = 0.0
         CHACNT(I) = 0
         CHAWT(I) = 0.0
         CHASUM(I) = 0.0
 520     CONTINUE
C
      DO 580 K = 1,NOBS
C                                       1st antenna number (A)
         I = IJS(1,K)
C                                       2nd antenna number (B)
         J = IJS(2,K)
C                                       Complex numbers with phase equal
C                                       to twice the parallactic
C                                       angles.
         ZZA = CMPLX (COS (2.0*(CHI(1,K))), SIN (2.0*(CHI(1,K))))
         ZZB = CMPLX (COS (2.0*(CHI(2,K))), SIN (2.0*(CHI(2,K))))
C                                       Total intensity
         MODVIS = CMPLX (0.0, 0.0)
         DO 540 IMODEL=1,NMODEL
            MODVIS = MODVIS + VMOD(IMODEL,K)
 540        CONTINUE
         IF (WT(1,K).GT.0.0) THEN
            RESID = VOBS(1,K) - MODVIS
            RR = REAL (RESID)
            RI = AIMAG (RESID)
            RRTSUM = RRTSUM + WT(1,K) * (RR*RR + RI*RI)
            RRTWT = RRTWT + WT(1,K)
            PHASUM(I) = PHASUM(I) + WT(1,K) * (RR*RR + RI*RI)
            PHAWT(I) = PHAWT(I) + WT(1,K)
            PHASUM(J) = PHASUM(J) + WT(1,K) * (RR*RR + RI*RI)
            PHAWT(J) = PHAWT(J) + WT(1,K)
            END IF
         IF (WT(4,K).GT.0.0) THEN
            RESID = VOBS(4,K) - MODVIS
            RR = REAL (RESID)
            RI = AIMAG (RESID)
            LLTSUM = LLTSUM + WT(4,K) * (RR*RR + RI*RI)
            LLTWT = LLTWT + WT(4,K)
            PHASUM(I) = PHASUM(I) + WT(4,K) * (RR*RR + RI*RI)
            PHAWT(I) = PHAWT(I) + WT(4,K)
            PHASUM(J) = PHASUM(J) + WT(4,K) * (RR*RR + RI*RI)
            PHAWT(J) = PHAWT(J) + WT(4,K)
            END IF
C
         IF (WT(1,K).LE.0.0 .AND. WT(4,K).LE.0.0) GO TO 580
C                                       Contribution from RL
         MODVIS = CMPLX (0.0, 0.0)
         DO 560 IMODEL=1,NMODEL
            MODVIS = MODVIS + P(IMODEL) * VMOD(IMODEL,K)
 560        CONTINUE
         IF (WT(2,K).GT.0.0) THEN
            IF (WT(4,K).GT.0.0) THEN
               T1 = D(1,I) * ZZA * VOBS(4,K)
            ELSE
               T1 = D(1,I) * ZZA * VOBS(1,K)
               END IF
            IF (WT(1,K).GT.0.0) THEN
               T2 = CONJG (D(2,J)) * ZZB * VOBS(1,K)
            ELSE
               T2 = CONJG (D(2,J)) * ZZB * VOBS(4,K)
               END IF
C                                       With feed correction
            RESID = VOBS(2,K) - MODVIS - T1 - T2
            RR = REAL (RESID)
            RI = AIMAG (RESID)
            CHTSUM = CHTSUM + WT(2,K) * (RR*RR + RI*RI)
            CHTWT = CHTWT + WT(2,K)
            CHACNT(I) = CHACNT(I) + 1
            CHASUM(I) = CHASUM(I) + WT(2,K) * (RR*RR + RI*RI)
            CHAWT(I) = CHAWT(I) + WT(2,K)
            CHACNT(J) = CHACNT(J) + 1
            CHASUM(J) = CHASUM(J) + WT(2,K) * (RR*RR + RI*RI)
            CHAWT(J) = CHAWT(J) + WT(2,K)
C                                       Without feed correction
            RESID = VOBS(2,K) - MODVIS
            RR = REAL (RESID)
            RI = AIMAG (RESID)
            CH0SUM = CH0SUM + WT(2,K) * (RR*RR + RI*RI)
            CH0WT = CH0WT + WT(2,K)
            END IF
C                                       Contribution from LR
         MODVIS = CMPLX (0.0, 0.0)
         DO 570 IMODEL=1,NMODEL
            MODVIS = MODVIS + P(IMODEL) * CONJG (VMOD(IMODEL,K))
 570        CONTINUE
         IF (WT(3,K).GT.0.0) THEN
            IF (WT(4,K).GT.0.0) THEN
               T1 = D(1,J) * ZZB * CONJG (VOBS(4,K))
            ELSE
               T1 = D(1,J) * ZZB * CONJG (VOBS(1,K))
               END IF
            IF (WT(1,K).GT.0.0) THEN
               T2 = CONJG (D(2,I)) * ZZA * CONJG (VOBS(1,K))
            ELSE
               T2 = CONJG (D(2,I)) * ZZA * CONJG (VOBS(4,K))
               END IF
C                                       With feed correction
            RESID = CONJG (VOBS(3,K)) - MODVIS - T1 - T2
            RR = REAL (RESID)
            RI = AIMAG (RESID)
            CHTSUM = CHTSUM + WT(3,K) * (RR*RR + RI*RI)
            CHTWT = CHTWT + WT(3,K)
            CHACNT(I) = CHACNT(I) + 1
            CHASUM(I) = CHASUM(I) + WT(3,K) * (RR*RR + RI*RI)
            CHAWT(I) = CHAWT(I) + WT(3,K)
            CHACNT(J) = CHACNT(J) + 1
            CHASUM(J) = CHASUM(J) + WT(3,K) * (RR*RR + RI*RI)
            CHAWT(J) = CHAWT(J) + WT(3,K)
C                                       Without feed correction
            RESID = VOBS(2,K) - MODVIS
            RR = REAL (RESID)
            RI = AIMAG (RESID)
            CH0SUM = CH0SUM + WT(3,K) * (RR*RR + RI*RI)
            CH0WT = CH0WT + WT(3,K)
            END IF
 580     CONTINUE
C                                       Print residuals.
      IF (NMODEL.GE.1) THEN
         WRITE (MSGTXT,1580)
         CALL MSGWRT (4)
         RMS1 = SQRT (RRTSUM / RRTWT)
         RMS2 = SQRT (LLTSUM / LLTWT)
         WRITE (MSGTXT,1582) RMS1, RMS2
         CALL MSGWRT (4)
         END IF
      RMS1 = SQRT (CHTSUM / CHTWT)
      WRITE (MSGTXT,1590) RMS1
      CALL MSGWRT (4)
      RMS1 = SQRT (CH0SUM / CH0WT)
      WRITE (MSGTXT,1592) RMS1
      CALL MSGWRT (4)
C                                       Average PA range
      AVGPAR = 0.0
      TNANT = 0
      DO 670 I=1,NANT
         IF (CHACNT(I).GT.0) THEN
            IF (PARNGE(2,I).LT.PARNGE(1,I))
     *         PARNGE(2,I) = PARNGE(2,I) + TWOPI
            AVGPAR = AVGPAR + PARNGE(2,I) - PARNGE(1,I)
            TNANT = TNANT + 1
            END IF
 670     CONTINUE
      AVGPAR = AVGPAR / TNANT
      WRITE (MSGTXT,1595) AVGPAR * RAD2DG
      CALL MSGWRT (4)
C                                       Antenna based residuals
      WRITE (MSGTXT,1600)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1602)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1604)
      CALL MSGWRT (4)
      DO 680 I=1,NANT
         IF (CHACNT(I).GT.0) THEN
            RMS1 = SQRT (PHASUM(I) / PHAWT(I))
            RMS2 = SQRT (CHASUM(I) / CHAWT(I))
            WRITE (MSGTXT,1620) I, CHACNT(I), RMS1, RMS2,
     *         (PARNGE(2,I) - PARNGE(1,I)) * RAD2DG
            CALL MSGWRT (4)
            END IF
 680     CONTINUE
 999  RETURN
C-----------------------------------------------------------------------
 1240 FORMAT ('Interferometer Element',I4)
 1250 FORMAT (' R: Re = ',F7.4,' Im = ',F7.4,' Amp = ',F7.4,
     *   ' Phase(deg) = ',F7.2)
 1252 FORMAT (' L: Re = ',F7.4,' Im = ',F7.4,' Amp = ',F7.4,
     *   ' Phase(deg) = ',F7.2)
 1320 FORMAT ('Sub-model',I3)
 1330 FORMAT ('  Q+iU/I = (',F10.5,',',F10.5,')')
 1332 FORMAT ('  Frac. pol. = ',F7.5,', angle = ',F7.2)
 1580 FORMAT ('RMS residuals for the total intensity model:')
 1582 FORMAT ('  RR: ',F10.3,' Jy        LL: ',F10.3,' Jy')
 1590 FORMAT ('RMS post-fit cross-hand residual: ',F9.3,' Jy')
 1592 FORMAT ('The same without feed correction: ',F9.3,' Jy')
 1595 FORMAT ('Average parallactic angle range: ',F6.1,' deg')
 1600 FORMAT ('RMS residuals for baselines to specific antennas')
 1602 FORMAT ('  Ant.   #Vis.    Parallel   Cross-hand (Jy)  ',
     *        'PA range (deg)')
 1604 FORMAT ('  --------------------------------------------',
     *        '--------------')
 1620 FORMAT (I5,I8,F12.3,F13.3,F16.1)
      END
      SUBROUTINE CPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
*
*  -- LAPACK routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  CPOTRS solves a system of linear equations A*X = B with a Hermitian
*  positive definite matrix A using the Cholesky factorization
*  A = U**H*U or A = L*L**H computed by CPOTRF.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  A       (input) COMPLEX array, dimension (LDA,N)
*          The triangular factor U or L from the Cholesky factorization
*          A = U**H*U or A = L*L**H, as computed by CPOTRF.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  B       (input/output) COMPLEX array, dimension (LDB,NRHS)
*          On entry, the right hand side matrix B.
*          On exit, the solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX            ONE
      PARAMETER          ( ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           CTRSM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'CPOTRS', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
*
      IF( UPPER ) THEN
*
*        Solve A*X = B where A = U'*U.
*
*        Solve U'*X = B, overwriting B with X.
*
         CALL CTRSM( 'Left', 'Upper', 'Conjugate transpose', 'Non-unit',
     $               N, NRHS, ONE, A, LDA, B, LDB )
*
*        Solve U*X = B, overwriting B with X.
*
         CALL CTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, A, LDA, B, LDB )
      ELSE
*
*        Solve A*X = B where A = L*L'.
*
*        Solve L*X = B, overwriting B with X.
*
         CALL CTRSM( 'Left', 'Lower', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, A, LDA, B, LDB )
*
*        Solve L'*X = B, overwriting B with X.
*
         CALL CTRSM( 'Left', 'Lower', 'Conjugate transpose', 'Non-unit',
     $               N, NRHS, ONE, A, LDA, B, LDB )
      END IF
*
      RETURN
*
*     End of CPOTRS
*
      END
      SUBROUTINE CPOTRF( UPLO, N, A, LDA, INFO )
*
*  -- LAPACK routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      COMPLEX            A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  CPOTRF computes the Cholesky factorization of a complex Hermitian
*  positive definite matrix A.
*
*  The factorization has the form
*     A = U**H * U,  if UPLO = 'U', or
*     A = L  * L**H,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.
*
*  This is the block version of the algorithm, calling Level 3 BLAS.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX array, dimension (LDA,N)
*          On entry, the Hermitian matrix A.  If UPLO = 'U', the leading
*          N-by-N upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading N-by-N lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*
*          On exit, if INFO = 0, the factor U or L from the Cholesky
*          factorization A = U**H*U or A = L*L**H.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the leading minor of order i is not
*                positive definite, and the factorization could not be
*                completed.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE
      COMPLEX            CONE
      PARAMETER          ( ONE = 1.0E+0, CONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            J, JB, NB
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           CGEMM, CHERK, CPOTF2, CTRSM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'CPOTRF', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Determine the block size for this environment.
*
      NB = ILAENV( 1, 'CPOTRF', UPLO, N, -1, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.N ) THEN
*
*        Use unblocked code.
*
         CALL CPOTF2( UPLO, N, A, LDA, INFO )
      ELSE
*
*        Use blocked code.
*
         IF( UPPER ) THEN
*
*           Compute the Cholesky factorization A = U'*U.
*
            DO 10 J = 1, N, NB
*
*              Update and factorize the current diagonal block and test
*              for non-positive-definiteness.
*
               JB = MIN( NB, N-J+1 )
               CALL CHERK( 'Upper', 'Conjugate transpose', JB, J-1,
     $                     -ONE, A( 1, J ), LDA, ONE, A( J, J ), LDA )
               CALL CPOTF2( 'Upper', JB, A( J, J ), LDA, INFO )
               IF( INFO.NE.0 )
     $            GO TO 30
               IF( J+JB.LE.N ) THEN
*
*                 Compute the current block row.
*
                  CALL CGEMM( 'Conjugate transpose', 'No transpose', JB,
     $                        N-J-JB+1, J-1, -CONE, A( 1, J ), LDA,
     $                        A( 1, J+JB ), LDA, CONE, A( J, J+JB ),
     $                        LDA )
                  CALL CTRSM( 'Left', 'Upper', 'Conjugate transpose',
     $                        'Non-unit', JB, N-J-JB+1, CONE, A( J, J ),
     $                        LDA, A( J, J+JB ), LDA )
               END IF
   10       CONTINUE
*
         ELSE
*
*           Compute the Cholesky factorization A = L*L'.
*
            DO 20 J = 1, N, NB
*
*              Update and factorize the current diagonal block and test
*              for non-positive-definiteness.
*
               JB = MIN( NB, N-J+1 )
               CALL CHERK( 'Lower', 'No transpose', JB, J-1, -ONE,
     $                     A( J, 1 ), LDA, ONE, A( J, J ), LDA )
               CALL CPOTF2( 'Lower', JB, A( J, J ), LDA, INFO )
               IF( INFO.NE.0 )
     $            GO TO 30
               IF( J+JB.LE.N ) THEN
*
*                 Compute the current block column.
*
                  CALL CGEMM( 'No transpose', 'Conjugate transpose',
     $                        N-J-JB+1, JB, J-1, -CONE, A( J+JB, 1 ),
     $                        LDA, A( J, 1 ), LDA, CONE, A( J+JB, J ),
     $                        LDA )
                  CALL CTRSM( 'Right', 'Lower', 'Conjugate transpose',
     $                        'Non-unit', N-J-JB+1, JB, CONE, A( J, J ),
     $                        LDA, A( J+JB, J ), LDA )
               END IF
   20       CONTINUE
         END IF
      END IF
      GO TO 40
*
   30 CONTINUE
      INFO = INFO + J - 1
*
   40 CONTINUE
      RETURN
*
*     End of CPOTRF
*
      END
      SUBROUTINE CTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      COMPLEX            ALPHA
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  CTRSM  solves one of the matrix equations
*
*     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
*
*  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'   or   op( A ) = conjg( A' ).
*
*  The matrix X is overwritten on B.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry, SIDE specifies whether op( A ) appears on the left
*           or right of X as follows:
*
*              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
*
*              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix A is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n'   op( A ) = A.
*
*              TRANSA = 'T' or 't'   op( A ) = A'.
*
*              TRANSA = 'C' or 'c'   op( A ) = conjg( A' ).
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit triangular
*           as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of B. M must be at
*           least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of B.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  ALPHA  - COMPLEX         .
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  B need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - COMPLEX          array of DIMENSION ( LDA, k ), where k is m
*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*           upper triangular part of the array  A must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           A is not referenced.
*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*           lower triangular part of the array  A must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*           A  are not referenced either,  but are assumed to be  unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*           then LDA must be at least max( 1, n ).
*           Unchanged on exit.
*
*  B      - COMPLEX          array of DIMENSION ( LDB, n ).
*           Before entry,  the leading  m by n part of the array  B must
*           contain  the  right-hand  side  matrix  B,  and  on exit  is
*           overwritten by the solution matrix  X.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   LDB  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          CONJG, MAX
*     .. Local Scalars ..
      LOGICAL            LSIDE, NOCONJ, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      COMPLEX            TEMP
*     .. Parameters ..
      COMPLEX            ONE
      PARAMETER        ( ONE  = ( 1.0E+0, 0.0E+0 ) )
      COMPLEX            ZERO
      PARAMETER        ( ZERO = ( 0.0E+0, 0.0E+0 ) )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOCONJ = LSAME( TRANSA, 'T' )
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
*
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'CTRSM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*inv( A )*B.
*
            IF( UPPER )THEN
               DO 60, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 30, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   30                CONTINUE
                  END IF
                  DO 50, K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 40, I = 1, K - 1
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   40                   CONTINUE
                     END IF
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 100, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 70, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   70                CONTINUE
                  END IF
                  DO 90 K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 80, I = K + 1, M
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   80                   CONTINUE
                     END IF
   90             CONTINUE
  100          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*inv( A' )*B
*           or    B := alpha*inv( conjg( A' ) )*B.
*
            IF( UPPER )THEN
               DO 140, J = 1, N
                  DO 130, I = 1, M
                     TEMP = ALPHA*B( I, J )
                     IF( NOCONJ )THEN
                        DO 110, K = 1, I - 1
                           TEMP = TEMP - A( K, I )*B( K, J )
  110                   CONTINUE
                        IF( NOUNIT )
     $                     TEMP = TEMP/A( I, I )
                     ELSE
                        DO 120, K = 1, I - 1
                           TEMP = TEMP - CONJG( A( K, I ) )*B( K, J )
  120                   CONTINUE
                        IF( NOUNIT )
     $                     TEMP = TEMP/CONJG( A( I, I ) )
                     END IF
                     B( I, J ) = TEMP
  130             CONTINUE
  140          CONTINUE
            ELSE
               DO 180, J = 1, N
                  DO 170, I = M, 1, -1
                     TEMP = ALPHA*B( I, J )
                     IF( NOCONJ )THEN
                        DO 150, K = I + 1, M
                           TEMP = TEMP - A( K, I )*B( K, J )
  150                   CONTINUE
                        IF( NOUNIT )
     $                     TEMP = TEMP/A( I, I )
                     ELSE
                        DO 160, K = I + 1, M
                           TEMP = TEMP - CONJG( A( K, I ) )*B( K, J )
  160                   CONTINUE
                        IF( NOUNIT )
     $                     TEMP = TEMP/CONJG( A( I, I ) )
                     END IF
                     B( I, J ) = TEMP
  170             CONTINUE
  180          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*B*inv( A ).
*
            IF( UPPER )THEN
               DO 230, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 190, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  190                CONTINUE
                  END IF
                  DO 210, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 200, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  200                   CONTINUE
                     END IF
  210             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 220, I = 1, M
                        B( I, J ) = TEMP*B( I, J )
  220                CONTINUE
                  END IF
  230          CONTINUE
            ELSE
               DO 280, J = N, 1, -1
                  IF( ALPHA.NE.ONE )THEN
                     DO 240, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  240                CONTINUE
                  END IF
                  DO 260, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 250, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  250                   CONTINUE
                     END IF
  260             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 270, I = 1, M
                       B( I, J ) = TEMP*B( I, J )
  270                CONTINUE
                  END IF
  280          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*inv( A' )
*           or    B := alpha*B*inv( conjg( A' ) ).
*
            IF( UPPER )THEN
               DO 330, K = N, 1, -1
                  IF( NOUNIT )THEN
                     IF( NOCONJ )THEN
                        TEMP = ONE/A( K, K )
                     ELSE
                        TEMP = ONE/CONJG( A( K, K ) )
                     END IF
                     DO 290, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  290                CONTINUE
                  END IF
                  DO 310, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        IF( NOCONJ )THEN
                           TEMP = A( J, K )
                        ELSE
                           TEMP = CONJG( A( J, K ) )
                        END IF
                        DO 300, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  300                   CONTINUE
                     END IF
  310             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 320, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  320                CONTINUE
                  END IF
  330          CONTINUE
            ELSE
               DO 380, K = 1, N
                  IF( NOUNIT )THEN
                     IF( NOCONJ )THEN
                        TEMP = ONE/A( K, K )
                     ELSE
                        TEMP = ONE/CONJG( A( K, K ) )
                     END IF
                     DO 340, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  340                CONTINUE
                  END IF
                  DO 360, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        IF( NOCONJ )THEN
                           TEMP = A( J, K )
                        ELSE
                           TEMP = CONJG( A( J, K ) )
                        END IF
                        DO 350, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  350                   CONTINUE
                     END IF
  360             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 370, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  370                CONTINUE
                  END IF
  380          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of CTRSM .
*
      END
      SUBROUTINE CGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      COMPLEX            ALPHA, BETA
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  CGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X'   or   op( X ) = conjg( X' ),
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = conjg( A' ).
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = conjg( B' ).
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - COMPLEX         .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - COMPLEX          array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - COMPLEX          array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - COMPLEX         .
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - COMPLEX          array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          CONJG, MAX
*     .. Local Scalars ..
      LOGICAL            CONJA, CONJB, NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      COMPLEX            TEMP
*     .. Parameters ..
      COMPLEX            ONE
      PARAMETER        ( ONE  = ( 1.0E+0, 0.0E+0 ) )
      COMPLEX            ZERO
      PARAMETER        ( ZERO = ( 0.0E+0, 0.0E+0 ) )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     conjugated or transposed, set  CONJA and CONJB  as true if  A  and
*     B  respectively are to be  transposed but  not conjugated  and set
*     NROWA, NCOLA and  NROWB  as the number of rows and  columns  of  A
*     and the number of rows of  B  respectively.
*
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      CONJA = LSAME( TRANSA, 'C' )
      CONJB = LSAME( TRANSB, 'C' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     $         ( .NOT.CONJA                ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     $         ( .NOT.CONJB                ).AND.
     $         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'CGEMM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( NOTB )THEN
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B + beta*C.
*
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE IF( CONJA )THEN
*
*           Form  C := alpha*conjg( A' )*B + beta*C.
*
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + CONJG( A( L, I ) )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B + beta*C
*
            DO 150, J = 1, N
               DO 140, I = 1, M
                  TEMP = ZERO
                  DO 130, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  130             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  140          CONTINUE
  150       CONTINUE
         END IF
      ELSE IF( NOTA )THEN
         IF( CONJB )THEN
*
*           Form  C := alpha*A*conjg( B' ) + beta*C.
*
            DO 200, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 160, I = 1, M
                     C( I, J ) = ZERO
  160             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 170, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  170             CONTINUE
               END IF
               DO 190, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*CONJG( B( J, L ) )
                     DO 180, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  180                CONTINUE
                  END IF
  190          CONTINUE
  200       CONTINUE
         ELSE
*
*           Form  C := alpha*A*B'          + beta*C
*
            DO 250, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 210, I = 1, M
                     C( I, J ) = ZERO
  210             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 220, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  220             CONTINUE
               END IF
               DO 240, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 230, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  230                CONTINUE
                  END IF
  240          CONTINUE
  250       CONTINUE
         END IF
      ELSE IF( CONJA )THEN
         IF( CONJB )THEN
*
*           Form  C := alpha*conjg( A' )*conjg( B' ) + beta*C.
*
            DO 280, J = 1, N
               DO 270, I = 1, M
                  TEMP = ZERO
                  DO 260, L = 1, K
                     TEMP = TEMP + CONJG( A( L, I ) )*CONJG( B( J, L ) )
  260             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  270          CONTINUE
  280       CONTINUE
         ELSE
*
*           Form  C := alpha*conjg( A' )*B' + beta*C
*
            DO 310, J = 1, N
               DO 300, I = 1, M
                  TEMP = ZERO
                  DO 290, L = 1, K
                     TEMP = TEMP + CONJG( A( L, I ) )*B( J, L )
  290             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  300          CONTINUE
  310       CONTINUE
         END IF
      ELSE
         IF( CONJB )THEN
*
*           Form  C := alpha*A'*conjg( B' ) + beta*C
*
            DO 340, J = 1, N
               DO 330, I = 1, M
                  TEMP = ZERO
                  DO 320, L = 1, K
                     TEMP = TEMP + A( L, I )*CONJG( B( J, L ) )
  320             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  330          CONTINUE
  340       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B' + beta*C
*
            DO 370, J = 1, N
               DO 360, I = 1, M
                  TEMP = ZERO
                  DO 350, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  350             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  360          CONTINUE
  370       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of CGEMM .
*
      END
      SUBROUTINE CHERK ( UPLO, TRANS, N, K, ALPHA, A, LDA,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO, TRANS
      INTEGER            N, K, LDA, LDC
      REAL               ALPHA, BETA
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  CHERK  performs one of the hermitian rank k operations
*
*     C := alpha*A*conjg( A' ) + beta*C,
*
*  or
*
*     C := alpha*conjg( A' )*A + beta*C,
*
*  where  alpha and beta  are  real scalars,  C is an  n by n  hermitian
*  matrix and  A  is an  n by k  matrix in the  first case and a  k by n
*  matrix in the second case.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On  entry,   UPLO  specifies  whether  the  upper  or  lower
*           triangular  part  of the  array  C  is to be  referenced  as
*           follows:
*
*              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
*                                  is to be referenced.
*
*              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
*                                  is to be referenced.
*
*           Unchanged on exit.
*
*  TRANS  - CHARACTER*1.
*           On entry,  TRANS  specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   C := alpha*A*conjg( A' ) + beta*C.
*
*              TRANS = 'C' or 'c'   C := alpha*conjg( A' )*A + beta*C.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N specifies the order of the matrix C.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
*           of  columns   of  the   matrix   A,   and  on   entry   with
*           TRANS = 'C' or 'c',  K  specifies  the number of rows of the
*           matrix A.  K must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - COMPLEX          array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
*           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by n  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
*           then  LDA must be at least  max( 1, n ), otherwise  LDA must
*           be at least  max( 1, k ).
*           Unchanged on exit.
*
*  BETA   - REAL            .
*           On entry, BETA specifies the scalar beta.
*           Unchanged on exit.
*
*  C      - COMPLEX          array of DIMENSION ( LDC, n ).
*           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
*           upper triangular part of the array C must contain the upper
*           triangular part  of the  hermitian matrix  and the strictly
*           lower triangular part of C is not referenced.  On exit, the
*           upper triangular part of the array  C is overwritten by the
*           upper triangular part of the updated matrix.
*           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
*           lower triangular part of the array C must contain the lower
*           triangular part  of the  hermitian matrix  and the strictly
*           upper triangular part of C is not referenced.  On exit, the
*           lower triangular part of the array  C is overwritten by the
*           lower triangular part of the updated matrix.
*           Note that the imaginary parts of the diagonal elements need
*           not be set,  they are assumed to be zero,  and on exit they
*           are set to zero.
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, n ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, CONJG, MAX, REAL
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I, INFO, J, L, NROWA
      REAL               RTEMP
      COMPLEX            TEMP
*     .. Parameters ..
      REAL               ONE ,         ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      IF( LSAME( TRANS, 'N' ) )THEN
         NROWA = N
      ELSE
         NROWA = K
      END IF
      UPPER = LSAME( UPLO, 'U' )
*
      INFO = 0
      IF(      ( .NOT.UPPER               ).AND.
     $         ( .NOT.LSAME( UPLO , 'L' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.LSAME( TRANS, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANS, 'C' ) )      )THEN
         INFO = 2
      ELSE IF( N  .LT.0               )THEN
         INFO = 3
      ELSE IF( K  .LT.0               )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 7
      ELSE IF( LDC.LT.MAX( 1, N     ) )THEN
         INFO = 10
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'CHERK ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         IF( UPPER )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 20, J = 1, N
                  DO 10, I = 1, J
                     C( I, J ) = ZERO
   10             CONTINUE
   20          CONTINUE
            ELSE
               DO 40, J = 1, N
                  DO 30, I = 1, J - 1
                     C( I, J ) = BETA*C( I, J )
   30             CONTINUE
                  C( J, J ) = BETA*REAL( C( J, J ) )
   40          CONTINUE
            END IF
         ELSE
            IF( BETA.EQ.ZERO )THEN
               DO 60, J = 1, N
                  DO 50, I = J, N
                     C( I, J ) = ZERO
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 80, J = 1, N
                  C( J, J ) = BETA*REAL( C( J, J ) )
                  DO 70, I = J + 1, N
                     C( I, J ) = BETA*C( I, J )
   70             CONTINUE
   80          CONTINUE
            END IF
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  C := alpha*A*conjg( A' ) + beta*C.
*
         IF( UPPER )THEN
            DO 130, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 90, I = 1, J
                     C( I, J ) = ZERO
   90             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 100, I = 1, J - 1
                     C( I, J ) = BETA*C( I, J )
  100             CONTINUE
                  C( J, J ) = BETA*REAL( C( J, J ) )
               END IF
               DO 120, L = 1, K
                  IF( A( J, L ).NE.CMPLX( ZERO ) )THEN
                     TEMP = ALPHA*CONJG( A( J, L ) )
                     DO 110, I = 1, J - 1
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  110                CONTINUE
                     C( J, J ) = REAL( C( J, J )      ) +
     $                           REAL( TEMP*A( I, L ) )
                  END IF
  120          CONTINUE
  130       CONTINUE
         ELSE
            DO 180, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 140, I = J, N
                     C( I, J ) = ZERO
  140             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  C( J, J ) = BETA*REAL( C( J, J ) )
                  DO 150, I = J + 1, N
                     C( I, J ) = BETA*C( I, J )
  150             CONTINUE
               END IF
               DO 170, L = 1, K
                  IF( A( J, L ).NE.CMPLX( ZERO ) )THEN
                     TEMP      = ALPHA*CONJG( A( J, L ) )
                     C( J, J ) = REAL( C( J, J )      )   +
     $                           REAL( TEMP*A( J, L ) )
                     DO 160, I = J + 1, N
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  160                CONTINUE
                  END IF
  170          CONTINUE
  180       CONTINUE
         END IF
      ELSE
*
*        Form  C := alpha*conjg( A' )*A + beta*C.
*
         IF( UPPER )THEN
            DO 220, J = 1, N
               DO 200, I = 1, J - 1
                  TEMP = ZERO
                  DO 190, L = 1, K
                     TEMP = TEMP + CONJG( A( L, I ) )*A( L, J )
  190             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  200          CONTINUE
               RTEMP = ZERO
               DO 210, L = 1, K
                  RTEMP = RTEMP + CONJG( A( L, J ) )*A( L, J )
  210          CONTINUE
               IF( BETA.EQ.ZERO )THEN
                  C( J, J ) = ALPHA*RTEMP
               ELSE
                  C( J, J ) = ALPHA*RTEMP + BETA*REAL( C( J, J ) )
               END IF
  220       CONTINUE
         ELSE
            DO 260, J = 1, N
               RTEMP = ZERO
               DO 230, L = 1, K
                  RTEMP = RTEMP + CONJG( A( L, J ) )*A( L, J )
  230          CONTINUE
               IF( BETA.EQ.ZERO )THEN
                  C( J, J ) = ALPHA*RTEMP
               ELSE
                  C( J, J ) = ALPHA*RTEMP + BETA*REAL( C( J, J ) )
               END IF
               DO 250, I = J + 1, N
                  TEMP = ZERO
                  DO 240, L = 1, K
                     TEMP = TEMP + CONJG( A( L, I ) )*A( L, J )
  240             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  250          CONTINUE
  260       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of CHERK .
*
      END
      SUBROUTINE CPOTF2( UPLO, N, A, LDA, INFO )
*
*  -- LAPACK routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      COMPLEX            A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  CPOTF2 computes the Cholesky factorization of a complex Hermitian
*  positive definite matrix A.
*
*  The factorization has the form
*     A = U' * U ,  if UPLO = 'U', or
*     A = L  * L',  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.
*
*  This is the unblocked version of the algorithm, calling Level 2 BLAS.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          Hermitian matrix A is stored.
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX array, dimension (LDA,N)
*          On entry, the Hermitian matrix A.  If UPLO = 'U', the leading
*          n by n upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading n by n lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*
*          On exit, if INFO = 0, the factor U or L from the Cholesky
*          factorization A = U'*U  or A = L*L'.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -k, the k-th argument had an illegal value
*          > 0: if INFO = k, the leading minor of order k is not
*               positive definite, and the factorization could not be
*               completed.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
      COMPLEX            CONE
      PARAMETER          ( CONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            J
      REAL               AJJ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      COMPLEX            CDOTC
      EXTERNAL           LSAME, CDOTC
*     ..
*     .. External Subroutines ..
      EXTERNAL           CGEMV, CLACGV, CSSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, REAL, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'CPOTF2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( UPPER ) THEN
*
*        Compute the Cholesky factorization A = U'*U.
*
         DO 10 J = 1, N
*
*           Compute U(J,J) and test for non-positive-definiteness.
*
            AJJ = REAL( A( J, J ) ) - CDOTC( J-1, A( 1, J ), 1,
     $            A( 1, J ), 1 )
            IF( AJJ.LE.ZERO ) THEN
               A( J, J ) = AJJ
               GO TO 30
            END IF
            AJJ = SQRT( AJJ )
            A( J, J ) = AJJ
*
*           Compute elements J+1:N of row J.
*
            IF( J.LT.N ) THEN
               CALL CLACGV( J-1, A( 1, J ), 1 )
               CALL CGEMV( 'Transpose', J-1, N-J, -CONE, A( 1, J+1 ),
     $                     LDA, A( 1, J ), 1, CONE, A( J, J+1 ), LDA )
               CALL CLACGV( J-1, A( 1, J ), 1 )
               CALL CSSCAL( N-J, ONE / AJJ, A( J, J+1 ), LDA )
            END IF
   10    CONTINUE
      ELSE
*
*        Compute the Cholesky factorization A = L*L'.
*
         DO 20 J = 1, N
*
*           Compute L(J,J) and test for non-positive-definiteness.
*
            AJJ = REAL( A( J, J ) ) - CDOTC( J-1, A( J, 1 ), LDA,
     $            A( J, 1 ), LDA )
            IF( AJJ.LE.ZERO ) THEN
               A( J, J ) = AJJ
               GO TO 30
            END IF
            AJJ = SQRT( AJJ )
            A( J, J ) = AJJ
*
*           Compute elements J+1:N of column J.
*
            IF( J.LT.N ) THEN
               CALL CLACGV( J-1, A( J, 1 ), LDA )
               CALL CGEMV( 'No transpose', N-J, J-1, -CONE, A( J+1, 1 ),
     $                     LDA, A( J, 1 ), LDA, CONE, A( J+1, J ), 1 )
               CALL CLACGV( J-1, A( J, 1 ), LDA )
               CALL CSSCAL( N-J, ONE / AJJ, A( J+1, J ), 1 )
            END IF
   20    CONTINUE
      END IF
      GO TO 40
*
   30 CONTINUE
      INFO = J
*
   40 CONTINUE
      RETURN
*
*     End of CPOTF2
*
      END
      SUBROUTINE CGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      COMPLEX            ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  CGEMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,   or
*
*     y := alpha*conjg( A' )*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*conjg( A' )*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - COMPLEX         .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - COMPLEX          array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*  X      - COMPLEX          array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - COMPLEX         .
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - COMPLEX          array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry with BETA non-zero, the incremented array Y
*           must contain the vector y. On exit, Y is overwritten by the
*           updated vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      COMPLEX            ONE
      PARAMETER        ( ONE  = ( 1.0E+0, 0.0E+0 ) )
      COMPLEX            ZERO
      PARAMETER        ( ZERO = ( 0.0E+0, 0.0E+0 ) )
*     .. Local Scalars ..
      COMPLEX            TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
      LOGICAL            NOCONJ
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          CONJG, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'CGEMV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
      NOCONJ = LSAME( TRANS, 'T' )
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
*     First form  y := beta*y.
*
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  y := alpha*A*x + y.
*
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
*
*        Form  y := alpha*A'*x + y  or  y := alpha*conjg( A' )*x + y.
*
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 110, J = 1, N
               TEMP = ZERO
               IF( NOCONJ )THEN
                  DO 90, I = 1, M
                     TEMP = TEMP + A( I, J )*X( I )
   90             CONTINUE
               ELSE
                  DO 100, I = 1, M
                     TEMP = TEMP + CONJG( A( I, J ) )*X( I )
  100             CONTINUE
               END IF
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  110       CONTINUE
         ELSE
            DO 140, J = 1, N
               TEMP = ZERO
               IX   = KX
               IF( NOCONJ )THEN
                  DO 120, I = 1, M
                     TEMP = TEMP + A( I, J )*X( IX )
                     IX   = IX   + INCX
  120             CONTINUE
               ELSE
                  DO 130, I = 1, M
                     TEMP = TEMP + CONJG( A( I, J ) )*X( IX )
                     IX   = IX   + INCX
  130             CONTINUE
               END IF
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  140       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of CGEMV .
*
      END
      SUBROUTINE CLACGV( N, X, INCX )
*
*  -- LAPACK auxiliary routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
*     ..
*     .. Array Arguments ..
      COMPLEX            X( * )
*     ..
*
*  Purpose
*  =======
*
*  CLACGV conjugates a complex vector of length N.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The length of the vector X.  N >= 0.
*
*  X       (input/output) COMPLEX array, dimension
*                         (1+(N-1)*abs(INCX))
*          On entry, the vector of length N to be conjugated.
*          On exit, X is overwritten with conjg(X).
*
*  INCX    (input) INTEGER
*          The spacing between successive elements of X.
*
* =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IOFF
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CONJG
*     ..
*     .. Executable Statements ..
*
      IF( INCX.EQ.1 ) THEN
         DO 10 I = 1, N
            X( I ) = CONJG( X( I ) )
   10    CONTINUE
      ELSE
         IOFF = 1
         IF( INCX.LT.0 )
     $      IOFF = 1 - ( N-1 )*INCX
         DO 20 I = 1, N
            X( IOFF ) = CONJG( X( IOFF ) )
            IOFF = IOFF + INCX
   20    CONTINUE
      END IF
      RETURN
*
*     End of CLACGV
*
      END
      SUBROUTINE  CSSCAL(N,SA,CX,INCX)
C
C     scales a complex vector by a real constant.
C     jack dongarra, linpack, 3/11/78.
C     modified 3/93 to return if incx .le. 0.
C
      COMPLEX CX(1)
      REAL SA
      INTEGER I,INCX,N,NINCX
C
      IF( N.LE.0 .OR. INCX.LE.0 )RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        code for increment not equal to 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        CX(I) = CMPLX(SA*REAL(CX(I)),SA*AIMAG(CX(I)))
   10 CONTINUE
      RETURN
C
C        code for increment equal to 1
C
   20 DO 30 I = 1,N
        CX(I) = CMPLX(SA*REAL(CX(I)),SA*AIMAG(CX(I)))
   30 CONTINUE
      RETURN
      END
      LOGICAL          FUNCTION LSAME( CA, CB )
*
*  -- LAPACK auxiliary routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          CA, CB
*     ..
*
*  Purpose
*  =======
*
*  LSAME returns .TRUE. if CA is the same letter as CB regardless of
*  case.
*
*  Arguments
*  =========
*
*  CA      (input) CHARACTER*1
*  CB      (input) CHARACTER*1
*          CA and CB specify the single characters to be compared.
*
*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
*     ..
*     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
*     ..
*     .. Executable Statements ..
*
*     Test if the characters are equal
*
      LSAME = CA.EQ.CB
      IF( LSAME )
     $   RETURN
*
*     Now test for equivalence if both characters are alphabetic.
*
      ZCODE = ICHAR( 'Z' )
*
*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
*     machines, on which ICHAR returns a value with bit 8 set.
*     ICHAR('A') on Prime machines returns 193 which is the same as
*     ICHAR('A') on an EBCDIC machine.
*
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
*
      IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
*
*        ASCII is assumed - ZCODE is the ASCII code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
         IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
*
      ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
*
*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.
     $       INTA.GE.145 .AND. INTA.LE.153 .OR.
     $       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.
     $       INTB.GE.145 .AND. INTB.LE.153 .OR.
     $       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
*
      ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
*
*        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
*        plus 128 of either lower or upper case 'Z'.
*
         IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
      END IF
      LSAME = INTA.EQ.INTB
*
*     RETURN
*
*     End of LSAME
*
      END
      SUBROUTINE XERBLA( SRNAME, INFO )
*
*  -- LAPACK auxiliary routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            INFO
*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Arguments
*  =========
*
*  SRNAME  (input) CHARACTER*6
*          The name of the routine which called XERBLA.
*
*  INFO    (input) INTEGER
*          The position of the invalid parameter in the parameter list
*          of the calling routine.
*
*     .. Executable Statements ..
*
      WRITE( *, FMT = 9999 )SRNAME, INFO
*
      STOP
*
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
*
*     End of XERBLA
*
      END
      INTEGER          FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3,
     $                 N4 )
*
*  -- LAPACK auxiliary routine (preliminary version) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 20, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4
*     ..
*
*  Purpose
*  =======
*
*  ILAENV is called from the LAPACK routines to choose problem-dependent
*  parameters for the local environment.  See ISPEC for a description of
*  the parameters.
*
*  This version provides a set of parameters which should give good,
*  but not optimal, performance on many of the currently available
*  computers.  Users are encouraged to modify this subroutine to set
*  the tuning parameters for their particular machine using the option
*  and problem size information in the arguments.
*
*  This routine will not function correctly if it is converted to all
*  lower case.  Converting it to all upper case is allowed.
*
*  Arguments
*  =========
*
*  ISPEC   (input) INTEGER
*          Specifies the parameter to be returned as the value of
*          ILAENV.
*          = 1: the optimal blocksize; if this value is 1, an unblocked
*               algorithm will give the best performance.
*          = 2: the minimum block size for which the block routine
*               should be used; if the usable block size is less than
*               this value, an unblocked routine should be used.
*          = 3: the crossover point (in a block routine, for N less
*               than this value, an unblocked routine should be used)
*          = 4: the number of shifts, used in the nonsymmetric
*               eigenvalue routines
*          = 5: the minimum column dimension for blocking to be used;
*               rectangular blocks must have dimension at least k by m,
*               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
*          = 6: the crossover point for the SVD (when reducing an m by n
*               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
*               this value, a QR factorization is used first to reduce
*               the matrix to a triangular form.)
*          = 7: the number of processors
*          = 8: the crossover point for the multishift QR and QZ methods
*               for nonsymmetric eigenvalue problems.
*
*  NAME    (input) CHARACTER*(*)
*          The name of the calling subroutine, in either upper case or
*          lower case.
*
*  OPTS    (input) CHARACTER*(*)
*          The character options to the subroutine NAME, concatenated
*          into a single character string.  For example, UPLO = 'U',
*          TRANS = 'T', and DIAG = 'N' for a triangular routine would
*          be specified as OPTS = 'UTN'.
*
*  N1      (input) INTEGER
*  N2      (input) INTEGER
*  N3      (input) INTEGER
*  N4      (input) INTEGER
*          Problem dimensions for the subroutine NAME; these may not all
*          be required.
*
* (ILAENV) (output) INTEGER
*          >= 0: the value of the parameter specified by ISPEC
*          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The following conventions have been used when calling ILAENV from the
*  LAPACK routines:
*  1)  OPTS is a concatenation of all of the character options to
*      subroutine NAME, in the same order that they appear in the
*      argument list for NAME, even if they are not used in determining
*      the value of the parameter specified by ISPEC.
*  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
*      that they appear in the argument list for NAME.  N1 is used
*      first, N2 second, and so on, and unused problem dimensions are
*      passed a value of -1.
*  3)  The parameter value returned by ILAENV is checked for validity in
*      the calling subroutine.  For example, ILAENV is used to retrieve
*      the optimal blocksize for STRTRI as follows:
*
*      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
*      IF( NB.LE.1 ) NB = MAX( 1, N )
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            CNAME, SNAME
      CHARACTER*1        C1
      CHARACTER*2        C2, C4
      CHARACTER*3        C3
      CHARACTER*6        SUBNAM
      INTEGER            I, IC, IZ, NB, NBMIN, NX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL
*     ..
*     .. Executable Statements ..
*
      GO TO ( 100, 100, 100, 400, 500, 600, 700, 800 ) ISPEC
*
*     Invalid value for ISPEC
*
      ILAENV = -1
      RETURN
*
  100 CONTINUE
*
*     Convert NAME to upper case if the first character is lower case.
*
      ILAENV = 1
      SUBNAM = NAME
      IC = ICHAR( SUBNAM( 1:1 ) )
      IZ = ICHAR( 'Z' )
      IF( IZ.EQ.90 .OR. IZ.EQ.122 ) THEN
*
*        ASCII character set
*
         IF( IC.GE.97 .AND. IC.LE.122 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 10 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.97 .AND. IC.LE.122 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   10       CONTINUE
         END IF
*
      ELSE IF( IZ.EQ.233 .OR. IZ.EQ.169 ) THEN
*
*        EBCDIC character set
*
         IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $       ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $       ( IC.GE.162 .AND. IC.LE.169 ) ) THEN
            SUBNAM( 1:1 ) = CHAR( IC+64 )
            DO 20 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $             ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $             ( IC.GE.162 .AND. IC.LE.169 ) )
     $            SUBNAM( I:I ) = CHAR( IC+64 )
   20       CONTINUE
         END IF
*
      ELSE IF( IZ.EQ.218 .OR. IZ.EQ.250 ) THEN
*
*        Prime machines:  ASCII+128
*
         IF( IC.GE.225 .AND. IC.LE.250 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 30 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.225 .AND. IC.LE.250 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   30       CONTINUE
         END IF
      END IF
*
      C1 = SUBNAM( 1:1 )
      SNAME = C1.EQ.'S' .OR. C1.EQ.'D'
      CNAME = C1.EQ.'C' .OR. C1.EQ.'Z'
      IF( .NOT.( CNAME .OR. SNAME ) )
     $   RETURN
      C2 = SUBNAM( 2:3 )
      C3 = SUBNAM( 4:6 )
      C4 = C3( 2:3 )
*
      GO TO ( 110, 200, 300 ) ISPEC
*
  110 CONTINUE
*
*     ISPEC = 1:  block size
*
*     In these examples, separate code is provided for setting NB for
*     real and complex.  We assume that NB will take the same value in
*     single or double precision.
*
      NB = 1
*
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $            C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'PO' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NB = 1
         ELSE IF( SNAME .AND. C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            NB = 64
         ELSE IF( C3.EQ.'TRD' ) THEN
            NB = 1
         ELSE IF( C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         END IF
      ELSE IF( C2.EQ.'GB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'PB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'TR' ) THEN
         IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'LA' ) THEN
         IF( C3.EQ.'UUM' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'ST' ) THEN
         IF( C3.EQ.'EBZ' ) THEN
            NB = 1
         END IF
      END IF
      ILAENV = NB
      RETURN
*
  200 CONTINUE
*
*     ISPEC = 2:  minimum block size
*
      NBMIN = 2
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $       C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         END IF
      END IF
      ILAENV = NBMIN
      RETURN
*
  300 CONTINUE
*
*     ISPEC = 3:  crossover point
*
      NX = 0
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $       C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NX = 1
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NX = 1
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NX = 128
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NX = 128
            END IF
         END IF
      END IF
      ILAENV = NX
      RETURN
*
  400 CONTINUE
*
*     ISPEC = 4:  number of shifts (used by xHSEQR)
*
      ILAENV = 6
      RETURN
*
  500 CONTINUE
*
*     ISPEC = 5:  minimum column dimension (not used)
*
      ILAENV = 2
      RETURN
*
  600 CONTINUE
*
*     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
*
      ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 )
      RETURN
*
  700 CONTINUE
*
*     ISPEC = 7:  number of processors (not used)
*
      ILAENV = 1
      RETURN
*
  800 CONTINUE
*
*     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
*
      ILAENV = 50
      RETURN
*
*     End of ILAENV
*
      END
      COMPLEX FUNCTION CDOTC(N,CX,INCX,CY,INCY)
C
C     forms the dot product of two vectors, conjugating the first
C     vector.
C     jack dongarra, linpack,  3/11/78.
C
      COMPLEX CX(1),CY(1),CTEMP
      INTEGER I,INCX,INCY,IX,IY,N
C
      CTEMP = (0.0,0.0)
      CDOTC = (0.0,0.0)
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        code for unequal increments or equal increments
C          not equal to 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        CTEMP = CTEMP + CONJG(CX(IX))*CY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      CDOTC = CTEMP
      RETURN
C
C        code for both increments equal to 1
C
   20 DO 30 I = 1,N
        CTEMP = CTEMP + CONJG(CX(I))*CY(I)
   30 CONTINUE
      CDOTC = CTEMP
      RETURN
      END
