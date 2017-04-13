LOCAL INCLUDE 'XG2PL.INC'
      HOLLERITH XINAM1(3), XINAM2(3), XINCL1(2), XINCL2(2), XOUTXT(12),
     *   CATH2(256)
      REAL      XINSE1, XINSE2, XINDS1, XINDS2, XINVR1, XINVR2,
     *   APARM(10), BPARM(10), CPARM(10), XYRATO, XLTYPE, XDOTV, XGRCH
      COMMON /INPARM/ XINAM1, XINCL1, XINSE1, XINDS1, XINAM2, XINCL2,
     *   XINSE2, XINDS2, XINVR1, XINVR2, APARM, BPARM, CPARM, XOUTXT,
     *   XYRATO, XLTYPE, XDOTV, XGRCH
      INTEGER   INSEQ1, INSEQ2, INDSK1, INDSK2, INCNO1, INCNO2, INVER1,
     *   INVER2, LABEL, GRCHN, TVCHN, CAT2(256), SCRTCH(256), NPARMS,
     *   LX, LY, IDX, IDY, IX1, IX2, IY1, IY2
      LOGICAL   DOVPOL, PLOTV, PLOTI, DOTV
      DOUBLE PRECISION CATD2(128)
      EQUIVALENCE (CATD2, CATH2, CAT2)
      CHARACTER INAME1*12, INAME2*12, INCLS1*6, INCLS2*6, OUTEXT*48
      COMMON /XGPRMS/ CATD2, SCRTCH, INSEQ1, INSEQ2, INDSK1, INDSK2,
     *   INCNO1, INCNO2, INVER1, INVER2, LABEL, DOVPOL, PLOTV, PLOTI,
     *   DOTV, GRCHN, TVCHN, NPARMS, LX, LY, IDX, IDY, IX1, IX2, IY1,
     *   IY2
      COMMON /CHPRMS/ INAME1, INAME2, INCLS1, INCLS2, OUTEXT
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
LOCAL END
LOCAL INCLUDE 'XGTABLE.INC'
      INTEGER   MAXGAU, MAXPX
      PARAMETER (MAXGAU=8)
      PARAMETER (MAXPX=100)
C
      INTEGER   IBLC(2), ITRC(2), JBLC(2), JTRC(2), XGBUFF(512),
     *   ZEBUFF(512), XGKOLS(11), XGNUMV(11), ZEKOLS(10), ZENUMV(10),
     *   DOGAUS, PRTPIX(2,MAXPX), NUMPX, MAXGGA, MAXVGA
      REAL      XBAR, PRTGV(2+3*MAXGAU,2,MAXPX), REFPIX, REFINC,
     *    PRTZV(1+MAXGAU,2,MAXPX)
      DOUBLE PRECISION VOFF, REFVAL
      CHARACTER REFTYP*8
      COMMON /XGTABP/ VOFF, REFVAL, REFPIX, REFINC, XBAR, PRTGV, PRTZV,
     *   PRTPIX, XGBUFF, ZEBUFF, XGKOLS, XGNUMV, ZEKOLS, ZENUMV, IBLC,
     *   ITRC, JBLC, JTRC, DOGAUS, NUMPX, MAXGGA, MAXVGA
      COMMON /XGTABC/ REFTYP
LOCAL END
LOCAL INCLUDE 'XGPLOT.INC'
      INTEGER   PLBUFF(256), PLUN, PIND, TVCORN(2)
      REAL      BLC(2), TRC(2), LINT
      COMMON /XGPLOT/ PLBUFF, PLUN, PIND, BLC, TRC, LINT, TVCORN
LOCAL END
      PROGRAM XG2PL
C-----------------------------------------------------------------------
C! Task to create a plot (PL) file from XGAUS/ZEMAN results
C# Plot-util EXT-util
C-----------------------------------------------------------------------
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
C   XG2PL create a plot file of XGAUS/ZEMAN results
C   Inputs:   (from AIPS)
C      INNAME     H(3)   name of primary file.Ipol
C      INCLASS    H(2)   class of primary file.Ipol
C      INSEQ      R      sequence number of primary file. Ipol
C      INDISK     R      disk volume number. 0 means try all.Ipol
C      IN2NAME    H(3)   name of Vpol file.Ipol
C      IN2CLASS   H(2)   class of Vpol file.Ipol
C      IN2SEQ     R      sequence number of Vpol file. Ipol
C      IN2DISK    R      disk volume number. 0 means try all.Ipol
C      INVERS     R      version number of XG file, 0 means latest
C      IN2VERS    R      version number of ZE file, 0 latest
C      APARM      R(10)  Choose pixel, scaling, spectral BLC/TRC,
C                           average box
C      BPARM      R(10)  Choose what to plot
C      OUTTEXT    H(12)  Output text of data
C      XYRATIO    R      X/Y plot ratio
C      LTYPE      R      1=no labeling 2 no ticks, 3 RA/dec
C                        4=center rel., 5=subslice center-rel
C      DOTV       R      > 0 => TV, else plot file
C      GRCHAN     R      graphics channel to use
C-----------------------------------------------------------------------
      INTEGER   IRET, NCHAN, NWORDS, NP
      CHARACTER PRGNAM*6
      REAL      DBUFF(2), VBUFF(2), RBUFF(2), SBUFF(2), WBUFF(2),
     *   XBUFF(2)
      LONGINT   PDBUFF, PVBUFF, PRBUFF, PSBUFF, PWBUFF, PXBUFF
C      REAL      DBUFF(10000), VBUFF(10000), RBUFF(10000), SBUFF(10000),
C     *   WBUFF(10000), XBUFF(10000)
      INCLUDE 'XG2PL.INC'
      INCLUDE 'XGTABLE.INC'
      DATA PRGNAM /'XG2PL'/
C-----------------------------------------------------------------------
C                                       Init
      CALL XG2PLI (PRGNAM, NCHAN, IRET)
C                                       allocate memory
      IF (IRET.EQ.0) THEN
         NP = 3 + MAXGAU
         NWORDS = (NP * NCHAN - 1) / 1024 + 4
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, DBUFF, PDBUFF, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, VBUFF,
     *      PVBUFF, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, RBUFF,
     *      PRBUFF, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SBUFF,
     *      PSBUFF, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, WBUFF,
     *      PWBUFF, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, XBUFF,
     *      PXBUFF, IRET)
         END IF
C                                       get data
      IF (IRET.EQ.0) CALL XG2PLG (NCHAN, NP, DBUFF(1+PDBUFF),
     *   VBUFF(1+PVBUFF), RBUFF(1+PRBUFF), SBUFF(1+PSBUFF),
     *   WBUFF(1+PWBUFF), XBUFF(1+PXBUFF), IRET)
C      IF (IRET.EQ.0) CALL XG2PLG (NCHAN, NP, DBUFF,
C     *   VBUFF, RBUFF, SBUFF,
C     *   WBUFF, XBUFF, IRET)
C                                       plot data
      IF (IRET.EQ.0) CALL XG2PLP (NCHAN, NP, DBUFF(1+PDBUFF),
     *   VBUFF(1+PVBUFF), IRET)
C      IF (IRET.EQ.0) CALL XG2PLP (NCHAN, NP, DBUFF,
C     *   VBUFF, IRET)
C                                       print data
      IF (IRET.EQ.0) CALL XG2PLH (NCHAN, NP, DBUFF(1+PDBUFF),
     *   VBUFF(1+PVBUFF), IRET)
C      IF (IRET.EQ.0) CALL XG2PLH (NCHAN, NP, DBUFF,
C     *   VBUFF, IRET)
c
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE XG2PLI (PRGNAM, NCHAN, IRET)
C-----------------------------------------------------------------------
C   Inits this task
C   Input:
C      PRGNAM   C*6   Task name
C   Outputs:
C      NCHAN    I     Number spectral channels
C      IRET     I     Error code: 0 okay
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*(*)
      INTEGER   NCHAN, IRET
C
      INTEGER   I, IDUM, LUN, LUNTMP, LTYPE, IZERNO, IXGRNO, IROUND
      REAL      DUM, ZEFPIX, ZEFINC
      DOUBLE PRECISION ZEFVAL
      CHARACTER MTYPE*2, STAT*4, ZEFTYP*8
      INCLUDE 'XG2PL.INC'
      INCLUDE 'XGTABLE.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
C                                       Get input values from AIPS.
      NPARMS = 62
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XINAM1, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         END IF
      IF (RQUICK) CALL RELPOP (IRET, scrtch, I)
      IF (IRET.NE.0) GO TO 999
C                                       Hollerith -> char.
      CALL H2CHR (48, 1 , XOUTXT, OUTEXT)
      CALL H2CHR (12, 1, XINAM1, INAME1)
      CALL H2CHR (6, 1, XINCL1, INCLS1)
      INSEQ1 = IROUND (XINSE1)
      INDSK1 = IROUND (XINDS1)
      CALL H2CHR (12, 1, XINAM2, INAME2)
      CALL H2CHR (6, 1, XINCL2, INCLS2)
      INSEQ2 = IROUND (XINSE2)
      INDSK2 = IROUND (XINDS2)
      INVER1 = IROUND (XINVR1)
      INVER2 = IROUND (XINVR2)
      LABEL = IROUND (XLTYPE)
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (LTYPE.GT.10)) THEN
         LTYPE = 3
         IF (LABEL.GE.0) THEN
            LABEL = (LABEL/100)*100 + LTYPE
         ELSE
            LABEL = (LABEL/100)*100 - LTYPE
            END IF
         END IF
      XLTYPE = LABEL
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
C                                       Zeman image
      DOVPOL = (INAME2.NE.' ') .AND. (INCLS2.NE.' ')
      IF (.NOT.DOVPOL) THEN
         BPARM(6) = 0.0
         BPARM(7) = 0.0
         BPARM(8) = 0.0
         BPARM(9) = 0.0
         INAME2 = ' '
         INCLS2 = ' '
         INSEQ2 = 0
         INDSK2 = 0
         INVER2 = 0
      ELSE
         INCNO2 = 1
         MTYPE = 'MA'
         CALL CATDIR ('SRCH', INDSK2, INCNO2, INAME2, INCLS2, INSEQ2,
     *      MTYPE, NLUSER, STAT, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET, INAME2, INCLS2, INSEQ2, INDSK2,
     *         NLUSER
            GO TO 990
            END IF
C                                       Read CATBLK and mark 'READ'.
         CALL CATIO ('READ', INDSK2, INCNO2, CATBLK, 'READ', SCRTCH,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING VPOL HEADER'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = INDSK2
         FCNO(NCFILE) = INCNO2
         FRW(NCFILE) = 0
         CALL COPY (256, CATBLK, CAT2)
C                                       Open ZE table
         CALL FNDEXT ('ZE', CATBLK, I)
         IF ((INVER2.LE.0) .OR. (INVER2.GT.I)) INVER2 = I
         IF (INVER2.LE.0) THEN
             BPARM(7) = 0.0
             BPARM(8) = 0.0
             BPARM(9) = 0.0
             JBLC(1) = 1
             JBLC(2) = 1
             JTRC(1) = CATBLK(KINAX+1)
             JTRC(2) = CATBLK(KINAX+2)
         ELSE
            LUN = LUNTMP (1)
            CALL ZEINI ('READ', ZEBUFF, INDSK2, INCNO2, INVER2, CATBLK,
     *         LUN, IZERNO, ZEKOLS, ZENUMV, JBLC, JTRC, DOGAUS, DUM,
     *         IDUM, ZEFVAL, ZEFPIX, ZEFINC, ZEFTYP, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPENING REQUESTED ZE TABLE'
               GO TO 990
               END IF
            END IF
         END IF
      CALL CHR2H (12, INAME2, 1, XINAM2)
      CALL CHR2H (6, INCLS2, 1, XINCL2)
      XINSE2 = INSEQ2
      XINDS2 = INDSK2
      XINVR2 = INVER2
C                                       IPOL
      INCNO1 = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', INDSK1, INCNO1, INAME1, INCLS1, INSEQ1,
     *   MTYPE, NLUSER, STAT, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, INAME1, INCLS1, INSEQ1, INDSK1,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      STAT = 'WRIT'
      IF (DOTV) STAT = 'READ'
      CALL CATIO ('READ', INDSK1, INCNO1, CATBLK, STAT, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING IPOL HEADER'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = INDSK1
      FCNO(NCFILE) = INCNO1
      FRW(NCFILE) = 1
      IF (DOTV) FRW(NCFILE) = 0
C                                       Open ZE table
      CALL FNDEXT ('XG', CATBLK, I)
      IF ((INVER1.LE.0) .OR. (INVER1.GT.I)) INVER1 = I
      IF (INVER1.LE.0) THEN
         BPARM(2) = 0.0
         BPARM(3) = 0.0
         BPARM(4) = 0.0
         IBLC(1) = 1
         IBLC(2) = 1
         ITRC(1) = CATBLK(KINAX+1)
         ITRC(2) = CATBLK(KINAX+2)
         REFPIX = CATR(KRCRP)
      ELSE
         LUN = LUNTMP (1)
         CALL XGINI ('READ', XGBUFF, INDSK1, INCNO1, INVER1, CATBLK,
     *      LUN, IXGRNO, XGKOLS, XGNUMV, IBLC, ITRC, IDUM, IDUM, DUM,
     *      VOFF, IDUM, REFVAL, REFPIX, REFINC, REFTYP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING REQUESTED XG TABLE'
            GO TO 990
            END IF
         IF (INVER2.GT.0) THEN
            IF ((REFVAL.NE.ZEFVAL) .OR. (REFPIX.NE.ZEFPIX) .OR.
     *         (REFINC.NE.ZEFINC) .OR. (REFTYP.NE.ZEFTYP)) THEN
               MSGTXT = 'WARNING: ZE FILE COORDINATES DO NOT MATCH' //
     *            ' XG FILE COORDINATES'
               CALL MSGWRT (7)
               END IF
            END IF
         END IF
      NCHAN = CATBLK(KINAX)
      XINVR1 = INVER1
C                                       channels to do
      IF (CPARM(1).LT.1.0) CPARM(1) = 1.0
      IF (CPARM(1).GT.NCHAN) CPARM(1) = 1.0
      IF (CPARM(2).LE.CPARM(1)) CPARM(2) = NCHAN
      IF (CPARM(2).GT.NCHAN) CPARM(2) = NCHAN
      IF ((CPARM(3).LE.0.05) .OR. (CPARM(3).GT.0.95)) CPARM(3) = 0.5
C                                       pixels to do
      LX = IROUND (APARM(1))
      LY = IROUND (APARM(2))
      IDX = IROUND (APARM(7))
      IDY = IROUND (APARM(8))
      IDY = MAX (0, IDY)
      IF (IDX.GE.0) THEN
         IX1 = MAX (IBLC(1), LX-IDX)
         IX2 = MIN (ITRC(1), LX+IDX)
      ELSE
         IX1 = MAX (IBLC(1), LX-IDY)
         IX2 = MIN (ITRC(1), LX+IDY)
         END IF
      IY1 = MAX (IBLC(2), LY-IDY)
      IY2 = MIN (ITRC(2), LY+IDY)
      IF ((LX.LT.IBLC(1)) .OR. (LX.GT.ITRC(1)) .OR. (LY.LT.IBLC(2)) .OR.
     *   (LY.GT.ITRC(2))) THEN
         IRET = 10
         MSGTXT = 'REQUESTED CENTER PIXEL NOT IN THE IPOL XG TABLE'
         GO TO 990
         END IF
      IF (DOVPOL) THEN
         IF ((LX.LT.JBLC(1)) .OR. (LX.GT.JTRC(1)) .OR. (LY.LT.JBLC(2))
     *      .OR. (LY.GT.JTRC(2))) THEN
            IRET = 10
            MSGTXT = 'REQUESTED CENTER PIXEL NOT IN THE VPOL ZE TABLE'
            GO TO 990
            END IF
         END IF
      XBAR = -REFPIX
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XG2PLI ERROR',I4,' ON ',A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
      END
      SUBROUTINE XG2PLG (NCHAN, NP, DBUFF, VBUFF, RBUFF, SBUFF, WBUFF,
     *   XBUFF, IRET)
C-----------------------------------------------------------------------
C   XG2PLG gets the data and models as much as it can for the desired
C   pixel
C   Inputs:
C      NCHAN   I      Number spectral channels
C      NP      I      Number of parameters
C   Output
C      DBUFF   R(*)   Ipol data, model, residual, model comps
C      VBUFF   R(*)   Vpol data, model, residual, model comps
C      RBUFF   R(*)   scratch
C      SBUFF   R(*)   scratch
C      WBUFF   R(*)   scratch
C      XBUFF   R(*)   scratch
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   NCHAN, NP, IRET
      REAL      DBUFF(NCHAN,*), VBUFF(NCHAN,*), RBUFF(NCHAN,*),
     *   SBUFF(NCHAN,*), WBUFF(NCHAN,*), XBUFF(NCHAN,*)
C
      INCLUDE 'XG2PL.INC'
      INCLUDE 'XGTABLE.INC'
      INTEGER   IX, IY, IC, IP, NG, NV
      REAL      DR
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = -1
      MAXGGA = 0
      MAXVGA = 0
C                                       single pixel
      IF ((APARM(7).EQ.0.0) .AND. (APARM(8).EQ.0.0)) THEN
         NUMPX = 1
         PRTPIX(1,NUMPX) = LX
         PRTPIX(2,NUMPX) = LY
         CALL XG2DAT (LX, LY, NCHAN, DBUFF, VBUFF, NG, NV, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       average over AREA
      ELSE
         IX = NCHAN * NP
         CALL RFILL (IX, 0.0, WBUFF)
         CALL RFILL (IX, 0.0, XBUFF)
         CALL RFILL (IX, 0.0, DBUFF)
         CALL RFILL (IX, 0.0, VBUFF)
         DR = -1
         NUMPX = 0
         DO 50 IY = IY1,IY2
            DO 40 IX = IX1,IX2
               IF (IDX.LT.0) THEN
                  DR = (IX-LX)*(IX-LX) + (IY-LY)*(IY-LY)
                  DR = SQRT (DR)
                  END IF
               IF (DR.LE.IDY) THEN
                  IF (NUMPX.LT.MAXPX) NUMPX = NUMPX + 1
                  PRTPIX(1,NUMPX) = IX
                  PRTPIX(2,NUMPX) = IY
                  CALL XG2DAT (IX, IY, NCHAN, RBUFF, SBUFF, NG, NV,
     *               IRET)
                  IF (IRET.NE.0) GO TO 999
                  IF (APARM(9).GT.0.0) THEN
                     NG = MAX (1, NG)
                     NV = MAX (1, NV)
                  END IF
                  DO 30 IP = 1,NP
                     DO 20 IC = 1,NCHAN
                        IF ((RBUFF(IC,IP).NE.FBLANK) .AND. (NG.GT.0))
     *                     THEN
                           WBUFF(IC,IP) = WBUFF(IC,IP) + 1.0
                           DBUFF(IC,IP) = DBUFF(IC,IP) + RBUFF(IC,IP)
                           END IF
                        IF ((SBUFF(IC,IP).NE.FBLANK) .AND. (NV.GT.0))
     *                     THEN
                           XBUFF(IC,IP) = XBUFF(IC,IP) + 1.0
                           VBUFF(IC,IP) = VBUFF(IC,IP) + SBUFF(IC,IP)
                           END IF
 20                     CONTINUE
 30                  CONTINUE
                  END IF
 40            CONTINUE
 50         CONTINUE
         DO 70 IP = 1,NP
            DO 60 IC = 1,NCHAN
               IF (WBUFF(IC,IP).GT.0.0) THEN
                  DBUFF(IC,IP) = DBUFF(IC,IP) / WBUFF(IC,IP)
                  IRET = 0
               ELSE
                  DBUFF(IC,IP) = FBLANK
                  END IF
               IF (XBUFF(IC,IP).GT.0.0) THEN
                  VBUFF(IC,IP) = VBUFF(IC,IP) / XBUFF(IC,IP)
                  IRET = 0
               ELSE
                  VBUFF(IC,IP) = FBLANK
                  END IF
 60            CONTINUE
 70         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE XG2DAT (MX, MY, NCHAN, DBUFF, VBUFF, NG, NV, IRET)
C-----------------------------------------------------------------------
C   Gets the data for 1 pixel
C   Inputs:
C      MX      I      X pixel (actually Y in cubes)
C      MY      I      Y pixel (actually Z in cubes)
C      NCHAN   I      Number spectral channels
C   Outputs:
C      DBUFF   R(*)   Ipol data, model, residual, components
C      VBUFF   R(*)   Vpol data, model, residual, components
C      NG      I      Number gaussians in model
C      NV      I      number comps in V model
C      IRET    I      Error
C-----------------------------------------------------------------------
      INTEGER   MX, MY, NCHAN, NG, NV, IRET
      REAL      DBUFF(NCHAN,*), VBUFF(NCHAN,*)
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'XG2PL.INC'
      INCLUDE 'XGTABLE.INC'
      INTEGER   LUN, IND, IDEPTH(5), WIN(4), IBIND, JBUFSZ, I, NP, BOI,
     *   YZPIX(2), J, K, IRNO
      REAL      BUFF1(UVBFSS), VPEAK, AMP, POS, SIG, DR, X,
     *   XGAUSV(3,MAXGAU), VANS(1+MAXGAU,2)
      CHARACTER PHNAME*48
      DOUBLE PRECISION HALFAC
      INCLUDE 'INCS:DHDR.INC'
      DATA HALFAC /2.77258872D0/
C-----------------------------------------------------------------------
      NP = 3 + MAXGAU
      I = NP * NCHAN
      CALL RFILL (I, FBLANK, DBUFF)
      CALL RFILL (I, FBLANK, VBUFF)
C                                       read Ipol spectrum
      WIN(1) = 1
      WIN(2) = MX
      WIN(3) = NCHAN
      WIN(4) = MX
      IDEPTH(1) = MY
      IDEPTH(2) = 1
      IDEPTH(3) = 1
      IDEPTH(4) = 1
      IDEPTH(5) = 1
      JBUFSZ = 2 * UVBFSS
      LUN = 33
      CALL ZPHFIL ('MA', INDSK1, INCNO1, 1, PHNAME, IRET)
      CALL ZOPEN (LUN, IND, INDSK1, PHNAME, .TRUE., .FALSE., .TRUE.,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN IPOL IMAGE'
         GO TO 990
         END IF
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, BOI, IRET)
      BOI = BOI + 1
      CALL MINIT ('READ', LUN, IND, CATBLK(KINAX), CATBLK(KINAX+1),
     *   WIN, BUFF1, JBUFSZ, BOI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT IO TO IPOL IMAGE'
         GO TO 990
         END IF
      CALL MDISK ('READ', LUN, IND, BUFF1, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ IPOL IMAGE'
         GO TO 990
         END IF
      CALL ZCLOSE (LUN, IND, IRET)
      CALL RCOPY (NCHAN, BUFF1(IBIND), DBUFF(1,1))
C                                       read Vpol spectrum
      IF (DOVPOL) THEN
         CALL ZPHFIL ('MA', INDSK2, INCNO2, 1, PHNAME, IRET)
         CALL ZOPEN (LUN, IND, INDSK2, PHNAME, .TRUE., .FALSE., .TRUE.,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN VPOL IMAGE'
            GO TO 990
            END IF
         CALL COMOFF (CAT2(KIDIM), CAT2(KINAX), IDEPTH, BOI, IRET)
         BOI = BOI + 1
         CALL MINIT ('READ', LUN, IND, CAT2(KINAX), CAT2(KINAX+1),
     *      WIN, BUFF1, JBUFSZ, BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT IO TO VPOL IMAGE'
            GO TO 990
            END IF
         CALL MDISK ('READ', LUN, IND, BUFF1, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ VPOL IMAGE'
            GO TO 990
            END IF
         CALL ZCLOSE (LUN, IND, IRET)
         CALL RCOPY (NCHAN, BUFF1(IBIND), VBUFF(1,1))
         END IF
C                                       Ipol model
      IF (INVER1.GT.0) THEN
         J = 2 * 3 * MAXGAU
         CALL RFILL (J, FBLANK, PRTGV(1,1,NUMPX))
         IRNO = (ITRC(1)-IBLC(1)+1) * (MY - IBLC(2)) + MX - IBLC(1)+1
         CALL TABXG ('READ', XGBUFF, IRNO, XGKOLS, XGNUMV, YZPIX, NG,
     *      VPEAK, PRTGV(1,1,NUMPX), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ XG TABLE'
            GO TO 990
            END IF
C                                       check consistent
         IF ((YZPIX(1).NE.MX) .OR. (YZPIX(2).NE.MY)) THEN
            WRITE (MSGTXT,1010) YZPIX, 'XG', MX, MY
            IRET = 10
            GO TO 990
            END IF
         CALL RFILL (NCHAN, 0.0, DBUFF(1,2))
         MAXGGA = MAX (MAXGGA, NG)
C                                       baseline
         IF ((PRTGV(1,1,NUMPX).NE.FBLANK) .AND.
     *      (PRTGV(2,1,NUMPX).NE.FBLANK)) THEN
            DO 20 I = 1,NCHAN
               DBUFF(I,2) = PRTGV(1,1,NUMPX) + PRTGV(2,1,NUMPX)*(I+XBAR)
 20            CONTINUE
            END IF
C                                       Gaussians
         DO 40 J = 1,NG
            K = 3 + 3 * (J-1)
            IF ((PRTGV(K,1,NUMPX).NE.FBLANK) .AND.
     *         (PRTGV(K+1,1,NUMPX).NE.FBLANK).AND.
     *         (PRTGV(K+2,1,NUMPX).NE.FBLANK)) THEN
               AMP = PRTGV(K,1,NUMPX)
               POS = PRTGV(K+1,1,NUMPX)
               SIG = PRTGV(K+2,1,NUMPX)
               DO 30 I = 1,NCHAN
                  DR = I + XBAR
                  DR = (DR - POS) / SIG
                  DR = HALFAC * DR * DR
                  IF (DR.LT.14.0) THEN
                     DBUFF(I,3+J) = AMP * EXP(-DR)
                     DBUFF(I,2) = DBUFF(I,2) + DBUFF(I,3+J)
                     END IF
 30               CONTINUE
               END IF
 40         CONTINUE
C                                       residual
         DO 50 I = 1,NCHAN
            IF ((DBUFF(I,1).NE.FBLANK) .AND. (DBUFF(I,2).NE.FBLANK))
     *         DBUFF(I,3) = DBUFF(I,1) - DBUFF(I,2)
 50         CONTINUE
         END IF
C                                       Vpol model
      IF ((DOVPOL) .AND. (INVER2.GT.0) .AND.
     *   ((DOGAUS.LE.0) .OR. (INVER1.GT.0))) THEN
         J = (1 * MAXGAU) * 2
         CALL RFILL (J, FBLANK, PRTZV(1,1,NUMPX))
         IRNO = (JTRC(1)-JBLC(1)+1) * (MY - JBLC(2)) + MX - JBLC(1)+1
         CALL TABZE ('READ', ZEBUFF, IRNO, ZEKOLS, ZENUMV, YZPIX,
     *      VPEAK, VANS, NV, XGAUSV, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ XG TABLE'
            GO TO 990
            END IF
C                                       check consistent
         IF ((YZPIX(1).NE.MX) .OR. (YZPIX(2).NE.MY)) THEN
            WRITE (MSGTXT,1010) YZPIX, 'XG', MX, MY
            IRET = 10
            GO TO 990
            END IF
         J = MAX (2, 1+NV)
         CALL RCOPY (J, VANS(1,1), PRTZV(1,1,NUMPX))
         CALL RCOPY (J, VANS(1,2), PRTZV(1,2,NUMPX))
         MAXVGA = MAX (MAXVGA, NV)
         IF (VANS(1,1).NE.FBLANK) THEN
            DO 60 I = 1,NCHAN
               VBUFF(I,2) = VANS(1,1) * DBUFF(I,1)
 60            CONTINUE
         ELSE
            CALL RFILL (NCHAN, 0.0, VBUFF(1,2))
            END IF
C                                       2-sided slope
         IF (DOGAUS.LT.0) THEN
            IF ((DBUFF(1,1).NE.FBLANK) .AND. (DBUFF(2,1).NE.FBLANK)
     *         .AND. (VANS(2,1).NE.FBLANK)) THEN
               VBUFF(1,4) = VANS(2,1) * (DBUFF(2,1)-DBUFF(1,1)) / 2.0
               VBUFF(1,2) = VBUFF(1,2) + VBUFF(1,4)
               IF (VANS(1,1).NE.FBLANK) VBUFF(1,4) = VBUFF(1,4) +
     *            VANS(1,1) * DBUFF(1,1)
               END IF
            IF ((DBUFF(NCHAN-1,1).NE.FBLANK) .AND. (VANS(2,1).NE.FBLANK)
     *         .AND. (DBUFF(NCHAN,1).NE.FBLANK)) THEN
               VBUFF(NCHAN,4) = VANS(2,1) * (DBUFF(NCHAN,1) -
     *            DBUFF(NCHAN-1,1)) / 2.0
               VBUFF(NCHAN,2) = VBUFF(NCHAN,2) + VBUFF(NCHAN,4)
               IF (VANS(1,1).NE.FBLANK) VBUFF(NCHAN,4) = VBUFF(NCHAN,4)
     *            + VANS(1,1) * DBUFF(NCHAN,1)
               END IF
            DO 70 I = 2,NCHAN-1
               IF ((DBUFF(I-1,1).NE.FBLANK) .AND. (VANS(2,1).NE.FBLANK)
     *            .AND. (DBUFF(I+1,1).NE.FBLANK)) THEN
                  VBUFF(I,4) = VANS(2,1) * (DBUFF(I+1,1) -
     *               DBUFF(I-1,1)) / 4.0
                  VBUFF(I,2) = VBUFF(I,2) + VBUFF(I,4)
                  IF (VANS(1,1).NE.FBLANK) VBUFF(I,4) = VBUFF(I,4) +
     *               VANS(1,1) * DBUFF(I,1)
                  END IF
 70            CONTINUE
C                                       one-sided
         ELSE IF (DOGAUS.EQ.0) THEN
            IF ((DBUFF(1,1).NE.FBLANK) .AND. (DBUFF(2,1).NE.FBLANK)
     *         .AND. (VANS(2,1).NE.FBLANK)) THEN
               VBUFF(1,4) = VANS(2,1) * (DBUFF(2,1)-DBUFF(1,1)) / 2.0
               VBUFF(1,2) = VBUFF(1,2) + VBUFF(1,4)
               IF (VANS(1,1).NE.FBLANK) VBUFF(1,4) = VBUFF(1,4) +
     *            VANS(1,1) * DBUFF(1,1)
               END IF
            IF ((DBUFF(NCHAN-1,1).NE.FBLANK) .AND. (VANS(2,1).NE.FBLANK)
     *         .AND. (DBUFF(NCHAN,1).NE.FBLANK)) THEN
               VBUFF(NCHAN,4) = VANS(2,1) * (DBUFF(NCHAN,1) -
     *            DBUFF(NCHAN-1,1)) / 2.0
               VBUFF(NCHAN,2) = VBUFF(NCHAN,2) + VBUFF(NCHAN,4)
               IF (VANS(1,1).NE.FBLANK) VBUFF(NCHAN,4) = VBUFF(NCHAN,4)
     *            + VANS(1,1) * DBUFF(NCHAN,1)
               END IF
            DO 80 I = 2,NCHAN-1
               IF ((DBUFF(I,1).NE.FBLANK) .AND. (VANS(2,1).NE.FBLANK)
     *            .AND. (DBUFF(I+1,1).NE.FBLANK)) THEN
                  VBUFF(I,4) = VANS(2,1) * (DBUFF(I+1,1)-DBUFF(I,1))/2.0
                  VBUFF(I,2) = VBUFF(I,2) + VBUFF(I,4)
                  IF (VANS(1,1).NE.FBLANK) VBUFF(I,4) = VBUFF(I,4) +
     *               VANS(1,1) * DBUFF(I,1)
                  END IF
 80            CONTINUE
C                                       Gaussians
         ELSE
            DO 90 J = 1,NV
               IF ((XGAUSV(1,J).NE.FBLANK) .AND. (XGAUSV(3,J).NE.0.0)
     *            .AND. (VANS(1+J,1).NE.FBLANK)) THEN
                  AMP = XGAUSV(1,J)
                  POS = XGAUSV(2,J)
                  SIG = ABS (XGAUSV(3,J))
                  DO 85 I = 1,NCHAN
                     X = I + XBAR
                     DR = (X - POS) / SIG
                     DR = HALFAC * DR * DR
                     IF (DR.LT.14.0) THEN
                        DR = AMP *  EXP (-DR)
                        VBUFF(I,3+J) = -DR * VANS(1+J,1) * HALFAC *
     *                     (X - POS) / (SIG * SIG)
                        VBUFF(I,2) = VBUFF(I,2) + VBUFF(I,3+J)
                        IF (VANS(1,1).NE.FBLANK) VBUFF(I,3+J) =
     *                     VBUFF(I,3+J) + VANS(1,1) * DBUFF(I,1)
                        END IF
 85                  CONTINUE
                  END IF
 90            CONTINUE
            END IF
         DO 95 I = 1,NCHAN
            IF ((VBUFF(I,1).NE.FBLANK) .AND. (VBUFF(I,2).NE.FBLANK))
     *         VBUFF(I,3) = VBUFF(I,1) - VBUFF(I,2)
 95         CONTINUE
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XG2DAT ERROR',I4,' ON ',A)
 1010 FORMAT ('XG2DAT FOUND PIXEL',2I6,' IN ',A2,' TABLE, WANTED',2I6)
      END
      SUBROUTINE XG2PLP (NCHAN, NP, DBUFF, VBUFF, IRET)
C-----------------------------------------------------------------------
C   XG2PLP does the requested plots
C   Inputs:
C      NCHAN   I      Number spectral channels
C      NP      I      Number parameters
C      DBUFF   R(*)   Ipol data
C      VBUFF   R(*)   Vpol data
C   Outputs:
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   NCHAN, NP, IRET
      REAL      DBUFF(NCHAN,*), VBUFF(NCHAN,*)
C
      INCLUDE 'XG2PL.INC'
      INCLUDE 'XGTABLE.INC'
      INCLUDE 'XGPLOT.INC'
      INTEGER   I, J, K, PVER, IERR, IDEPTH(5), IPSIZE, ITYPE, LTYPE,
     *   INP, JTRIM, INCHAR, IT(3), ID(3), BC, EC, NC
      LOGICAL   DOBOTH, GOOD, DOWN
      REAL      IMAX, IMIN, VMAX, VMIN, XYSCL(3), XYOFF(3), CHOUT(4),
     *   TEMP, TI, TR, X, Y, DX, DY, XX, XBLC(2), XTRC(2), YMIN, XXMIN
      CHARACTER PFILE*48, COMMNT*48, PTYPES(4)*10, TEXT*128, CTEMP*18,
     *   TIME*8, DATE*12
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA IDEPTH /5*1/
      DATA PTYPES /'data', 'model', 'residual', 'components'/
C-----------------------------------------------------------------------
      PLOTI = (BPARM(1).GT.0.0) .OR. (BPARM(2).GT.0.0) .OR.
     *   (BPARM(3).GT.0.0) .OR. (BPARM(4).GT.0.0)
      PLOTV = (BPARM(6).GT.0.0) .OR. (BPARM(7).GT.0.0) .OR.
     *   (BPARM(8).GT.0.0) .OR. (BPARM(9).GT.0.0)
      DOBOTH = PLOTI .AND. PLOTV
      IF ((.NOT.PLOTI) .AND. (.NOT.PLOTV)) THEN
         IRET = 0
         MSGTXT = 'NO PLOTS REQUESTED'
         CALL MSGWRT (7)
         GO TO 999
         END IF
      BC = CPARM(1) + 0.1
      EC = CPARM(2) + 0.1
      NC = EC - BC + 1
C                                       scaling
      IF ((PLOTI) .AND. (APARM(4).LE.APARM(3))) THEN
         IMAX = -1.E10
         IMIN = 1.E10
         DO 20 I = BC,EC
            DO 15 J = 1,NP
               K = MIN (4, J)
               IF (BPARM(K).GT.0.0) THEN
                  IF (DBUFF(I,J).NE.FBLANK) THEN
                     IMAX = MAX (IMAX, DBUFF(I,J))
                     IMIN = MIN (IMIN, DBUFF(I,J))
                     END IF
                  END IF
 15            CONTINUE
 20         CONTINUE
         APARM(4) = IMAX
         APARM(3) = IMIN
      ELSE IF (PLOTI) THEN
         IMAX = APARM(4)
         IMIN = APARM(3)
      ELSE
         APARM(4) = 1.0
         APARM(3) = -1.0
         END IF
      IF ((PLOTV) .AND. (APARM(6).LE.APARM(5))) THEN
         VMAX = -1.E10
         VMIN = 1.E10
         DO 30 I = BC,EC
            DO 25 J = 1,NP
               K = MIN (4, J) + 5
               IF (BPARM(K).GT.0.0) THEN
                  IF (VBUFF(I,J).NE.FBLANK) THEN
                     VMAX = MAX (VMAX, VBUFF(I,J))
                     VMIN = MIN (VMIN, VBUFF(I,J))
                     END IF
                  END IF
 25            CONTINUE
 30         CONTINUE
         APARM(6) = VMAX
         APARM(5) = VMIN
      ELSE IF (PLOTV) THEN
         VMAX = APARM(6)
         VMIN = APARM(5)
      ELSE
         APARM(6) = 1.0
         APARM(5) = -1.0
         END IF
C                                       Add PLot file to header
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', INDSK1, INCNO1, CATBLK, SCRTCH, .TRUE.,
     *      'READ', PVER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'ADDING PL FILE TO HEADER'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         FRW(NCFILE) = 0
         END IF
C                                       Create plot file
      CALL ZPHFIL ('PL', INDSK1, INCNO1, PVER, PFILE, IRET)
      IPSIZE = 0
      ITYPE = 48
      TVCORN(1) = 0
      TVCORN(2) = 0
      CALL GINIT (INDSK1, INCNO1, PFILE, IPSIZE, ITYPE, NPARMS, XINAM1,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLBUFF, PLUN, PIND, IRET)
      IF (IRET.NE.0) GO TO 980
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
C                                       Set up location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      DO 40 I = 1,3
         IF (I.EQ.1) THEN
            XYOFF(I) = BC
            XYSCL(I) = EC
         ELSE
            XYSCL(I) = APARM(2*I)
            XYOFF(I) = APARM(2*I-1)
            END IF
         TEMP = 0.03 * (XYSCL(I) - XYOFF(I))
         XYSCL(I) = XYSCL(I) + TEMP
         XYOFF(I) = XYOFF(I) - TEMP
         IF (XYSCL(I).EQ.XYOFF(I)) GO TO 999
         XYSCL(I) = 1000. / (XYSCL(I)-XYOFF(I))
 40      CONTINUE
      IF (.NOT.PLOTI) THEN
         LINT = 1000.0
         XYSCL(2) = XYSCL(3)
         XYOFF(2) = XYOFF(3)
      ELSE IF (.NOT.PLOTV) THEN
         LINT = 0.0
      ELSE
         LINT = 1000.0 * CPARM(3)
         XYSCL(2) = XYSCL(2) * (1.0 - CPARM(3))
         XYSCL(3) = XYSCL(3) * CPARM(3)
         END IF
C                                       characters to left
      XBLC(1) = BLC(1)
      XTRC(1) = TRC(1)
      INCHAR = 0
      IF ((PLOTI) .AND. (PLOTV)) THEN
C                                       axis labeling: lower plot
         XBLC(2) = BLC(2)
         XTRC(2) = LINT
         J = 1
         DO 45 I = 1,2
            TR = (XTRC(I)-XBLC(I)) / XYSCL(J)
            TI = TR
            IF ((I.EQ.1) .AND. (CPARM(5).GT.0.0)) THEN
               RPLOC(I,LOCNUM) = XTRC(I)
               AXINC(I,LOCNUM) = TR / (XBLC(I) - XTRC(I))
            ELSE
               RPLOC(I,LOCNUM) = XBLC(I)
               AXINC(I,LOCNUM) = TR / (XTRC(I) - XBLC(I))
               END IF
            RPVAL(I,LOCNUM) = XYOFF(J)
            CALL METSCL (LABEL, TR, CPREF(I,LOCNUM), GOOD)
            RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * TR / TI
            AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * TR / TI
            J = J + 2
 45         CONTINUE
         CALL CHNTIC (XBLC, XTRC, INCHAR)
         END IF
C                                       axis labeling: upper plot
      IF (PLOTI) THEN
         XBLC(2) = LINT
         XTRC(2) = TRC(2)
      ELSE
         XBLC(2) = BLC(2)
         XTRC(2) = TRC(2)
         END IF
      DO 50 I = 1,2
         TR = (XTRC(I)-XBLC(I)) / XYSCL(I)
         TI = TR
         IF ((I.EQ.1) .AND. (CPARM(5).GT.0.0)) THEN
            RPLOC(I,LOCNUM) = XTRC(I)
            AXINC(I,LOCNUM) = TR / (XBLC(I) - XTRC(I))
         ELSE
            RPLOC(I,LOCNUM) = XBLC(I)
            AXINC(I,LOCNUM) = TR / (XTRC(I) - XBLC(I))
            END IF
         RPVAL(I,LOCNUM) = XYOFF(I)
         CALL METSCL (LABEL, TR, CPREF(I,LOCNUM), GOOD)
         RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * TR / TI
         AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * TR / TI
 50      CONTINUE
      CALL CHNTIC (XBLC, XTRC, INP)
      INP = MAX (INP, INCHAR)
C                                       Count characters around
      CALL RFILL (4, 0.5, CHOUT)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
      IF ((LTYPE.GT.2) .AND. (INP.GT.0)) CHOUT(1) = 4.0 + INP
      IF (LTYPE.GT.1) CHOUT(2) = 2.0
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CHOUT(4) = 3.333
         IF (DOVPOL) CHOUT(4) = CHOUT(4) + 1.333
         IF (LABEL.GT.1) CHOUT(4) = CHOUT(4) + 1.333
         END IF
C                                       default XYRATIO
      IF (XYRATO.LT.0.01) THEN
         IF (DOTV) THEN
            XXMIN = WINDTV(3) - WINDTV(1) + 1 - CSIZTV(1) * (CHOUT(1)
     *         + CHOUT(3))
            YMIN = WINDTV(4) - WINDTV(2) + 1 - CSIZTV(2) * (CHOUT(2)
     *         + CHOUT(4))
            XYRATO = 1.0
            IF (YMIN.GT.0.0) XYRATO = XXMIN / YMIN
         ELSE
            XYRATO = 1.0
            END IF
         END IF
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, IDEPTH, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL GLTYPE (1, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Draw border
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (DOBOTH) THEN
         CALL GPOS (BLC(1), LINT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (TRC(1), LINT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
C                                       extra labels
      DX = 0.0
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) THEN
         DY = 0.5 + 2 * 1.333
         IF (DOVPOL) DY = DY + 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, TIME, DATE)
         WRITE (TEXT,1050) PVER, DATE, TIME
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
C                                       Top labels: type & name
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         DY = 0.5 + 1.333
         IF (DOVPOL) DY = DY + 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         INP = 21
         WRITE (TEXT,1051) 'XG', INVER1
         TEXT(INP-1:INP-1) = '_'
         CALL H2CHR (18, 1, CATH(KHIMN), CTEMP)
         CALL NAMEST (CTEMP, INSEQ1, TEXT(INP:), INCHAR)
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         IF (DOVPOL) THEN
            DY = DY - 1.333
            CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 970
            INP = 21
            WRITE (TEXT,1051) 'ZE', INVER2
            TEXT(INP-1:INP-1) = '_'
            CALL H2CHR (18, 1, CATH2(KHIMN), CTEMP)
            CALL NAMEST (CTEMP, INSEQ2, TEXT(INP:), INCHAR)
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 970
            END IF
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         IF ((IDX.EQ.0) .AND. (IDY.EQ.0)) THEN
            WRITE (TEXT,1060) LX, LY
         ELSE IF (IDX.LT.0) THEN
            WRITE (TEXT,1061) LX, LY, IDY
         ELSE
            WRITE (TEXT,1062) IX1, IX2, IY1, IY2
            END IF
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
C                                       axis labeling
      K = CPARM(4) + 1.1
      CALL XG2LAB (K, PLOTI, PLOTV, BLC, TRC, LINT, BC, EC, XYSCL,
     *   XYOFF, CHOUT, LABEL, XYRATO, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Plot I
      IF (PLOTI) THEN
         DO 140 J = 1,NP
            K = MIN (4, J)
            IF (BPARM(K).GT.0.0) THEN
               CALL GLTYPE (K, PLBUFF, IRET)
               IF (IRET.NE.0) GO TO 970
               COMMNT = 'Plot Ipol ' // PTYPES(K)
               INP = JTRIM (COMMNT)
               CALL GCOMNT (-1, COMMNT(:INP), PLBUFF, IRET)
               IF (IRET.NE.0) GO TO 970
               DOWN = .FALSE.
               DO 120 I = BC,EC
                  IF (DBUFF(I,J).NE.FBLANK) THEN
                     Y = XYSCL(2) * (DBUFF(I,J) - XYOFF(2)) + LINT
                     IF ((Y.GE.LINT) .AND. (Y.LE.TRC(2))) THEN
                        IF ((J.EQ.1) .AND. (CPARM(6).LE.0.0)) THEN
                           XX = I - 0.5 - XYOFF(1)
                           IF (CPARM(5).GT.0.0) XX = EC + BC - I + 0.5 -
     *                        XYOFF(1)
                           X = XYSCL(1) * XX
                           IF (DOWN) THEN
                              CALL GVEC (X, Y, PLBUFF, IRET)
                           ELSE
                              CALL GPOS (X, Y, PLBUFF, IRET)
                              DOWN = .TRUE.
                              END IF
                           IF (IRET.NE.0) GO TO 970
                           XX = I + 0.5 - XYOFF(1)
                           IF (CPARM(5).GT.0.0) XX = EC + BC - I - 0.5 -
     *                        XYOFF(1)
                           X = XYSCL(1) * XX
                           CALL GVEC (X, Y, PLBUFF, IRET)
                           IF (IRET.NE.0) GO TO 970
                        ELSE
                           XX = I - XYOFF(1)
                           IF (CPARM(5).GT.0.0) XX = EC + BC - I -
     *                        XYOFF(1)
                           X = XYSCL(1) * XX
                           IF (DOWN) THEN
                              CALL GVEC (X, Y, PLBUFF, IRET)
                           ELSE
                              CALL GPOS (X, Y, PLBUFF, IRET)
                              DOWN = .TRUE.
                              END IF
                           IF (IRET.NE.0) GO TO 970
                           END IF
                     ELSE
                        DOWN = .FALSE.
                        END IF
                  ELSE
                     DOWN = .FALSE.
                     END IF
 120              CONTINUE
               END IF
 140        CONTINUE
         END IF
      IF (PLOTV) THEN
         DO 190 J = 1,NP
            K = MIN (4, J)
            IF (BPARM(K+5).GT.0.0) THEN
               CALL GLTYPE (K, PLBUFF, IRET)
               IF (IRET.NE.0) GO TO 970
               COMMNT = 'Plot Vpol ' // PTYPES(K)
               INP = JTRIM (COMMNT)
               CALL GCOMNT (-1, COMMNT(:INP), PLBUFF, IRET)
               IF (IRET.NE.0) GO TO 970
               DOWN = .FALSE.
               DO 170 I = BC,EC
                  IF (VBUFF(I,J).NE.FBLANK) THEN
                     Y = XYSCL(3) * (VBUFF(I,J) - XYOFF(3))
                     IF ((Y.GE.BLC(2)) .AND. (Y.LE.LINT)) THEN
                        IF ((J.EQ.1) .AND. (CPARM(6).LE.0.0)) THEN
                           XX = I - 0.5 - XYOFF(1)
                           IF (CPARM(5).GT.0.0) XX = EC + BC - I + 0.5 -
     *                        XYOFF(1)
                           X = XYSCL(1) * XX
                           IF (DOWN) THEN
                              CALL GVEC (X, Y, PLBUFF, IRET)
                           ELSE
                              CALL GPOS (X, Y, PLBUFF, IRET)
                              DOWN = .TRUE.
                              END IF
                           IF (IRET.NE.0) GO TO 970
                           XX = I + 0.5 - XYOFF(1)
                           IF (CPARM(5).GT.0.0) XX = EC + BC - I - 0.5 -
     *                        XYOFF(1)
                           X = XYSCL(1) * XX
                           CALL GVEC (X, Y, PLBUFF, IRET)
                           IF (IRET.NE.0) GO TO 970
                        ELSE
                           XX = I - XYOFF(1)
                           IF (CPARM(5).GT.0.0) XX = EC + BC - I -
     *                        XYOFF(1)
                           X = XYSCL(1) * XX
                           IF (DOWN) THEN
                              CALL GVEC (X, Y, PLBUFF, IRET)
                           ELSE
                              CALL GPOS (X, Y, PLBUFF, IRET)
                              DOWN = .TRUE.
                              END IF
                           IF (IRET.NE.0) GO TO 970
                           END IF
                     ELSE
                        DOWN = .FALSE.
                        END IF
                  ELSE
                     DOWN = .FALSE.
                     END IF
 170              CONTINUE
               END IF
 190        CONTINUE
         END IF
C                                       finish plot
      CALL GFINIS (PLBUFF, IRET)
      IF (IRET.EQ.0) GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (PLBUFF, I)
      IF (I.EQ.0) GO TO 990
C                                       Destroy the plot file
 980  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (PLUN, PIND, IERR)
         CALL ZDESTR (INDSK1, PFILE, IERR)
         CALL DELEXT ('PL', INDSK1, INCNO1, 'READ', CATBLK, SCRTCH,
     *      PVER, IERR)
         END IF
      GO TO 999
C
 990  IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1993) PVER
         CALL MSGWRT (2)
         CALL HIPLOT (INDSK1, INCNO1, PVER, SCRTCH, I)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XG2PLP ERROR',I4,' ON ',A)
 1050 FORMAT ('Plot file version',I4,'__created ',A12,A8)
 1051 FORMAT ('Ext ',A2,'__Vers',I4)
 1060 FORMAT ('Single pixel at X =_',I6,'__Y =_',I6)
 1061 FORMAT ('Circle centered on X =_',I6,'__Y =_',I6,'__radius =_',
     *   I4)
 1062 FORMAT ('Rectangle X =_',I6,' - ',I6,'___Y =_',I6,' - ',I6)
 1970 FORMAT ('XG2PLP Error during graphing. will try to finish',
     *   ' partial graph')
 1993 FORMAT ('Plot file version',I5,'  created.')
      END
      SUBROUTINE XG2LAB (ITYP, PLOTI, PLOTV, BLC, TRC, LINT, BC, EC,
     *   XYSCL, XYOFF, CHOUT, LABEL, XYRATO, PLBUFF, IRET)
C-----------------------------------------------------------------------
C   XG2LAB handles the labeling
C   Inputs:
C      ITYP     I      2 freq, 3 velocity, else channels
C      PLOTI    L      Plot Ipol
C      PLOTV    L      Plot VPOL
C      BLC      R(2)   BLC of full plot area
C      TRC      R(2)   TRC of full plot area
C      LINT     R      Division between I and V
C      BC       I      First channel to plot
C      EC       I      Last channel to plot
C      XYSCL    R(3)   Plot scaling (for X = channels, I, V)
C      XYOFF    R(3)   Plot zero point
C      CHOUT    R(4)   Number characters left, bottom, right, top
C      LABEL    I      label type
C      XYRATO   R      X/Y ratio
C   In/out
C      PLBUFF   I(*)   Plot buffer
C   Output
C      IRET     I      Error code
C-----------------------------------------------------------------------
      LOGICAL   PLOTI, PLOTV
      REAL      BLC(2), TRC(2), LINT, XYSCL(3), XYOFF(3), CHOUT(4),
     *   XYRATO
      INTEGER   ITYP, BC, EC, LABEL, PLBUFF(*), IRET
C
      INTEGER   I, J
      REAL      INLOC(2), ININC(2), XBLC(2), XTRC(2), TI, TR, INSCL,
     *   INOFF, TEMP, DX, DY
      DOUBLE PRECISION INVAL(2), BVAL, EVAL, REFNU, DELNU, REFV, REFAP,
     *   VSIGN, DELV
      CHARACTER INPREF(2)*5, INTYP(2)*8, AXTYPE*8, SPREF*5, STYPE*8,
     *   VELREF(3)*4, POLAR*4
      LOGICAL   REVERS, GOOD
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA VELREF /'-LSR','-HEL','-OBS'/
C-----------------------------------------------------------------------
C                                       save inputs
      CTYP(1,LOCNUM) = 'Channels'
      INLOC(1) = RPLOC(1,LOCNUM)
      ININC(1) = AXINC(1,LOCNUM)
      INVAL(1) = RPVAL(1,LOCNUM)
      INPREF(1) = CPREF(1,LOCNUM)
      INTYP(1) = CTYP(1,LOCNUM)
      INLOC(2) = RPLOC(2,LOCNUM)
      ININC(2) = AXINC(2,LOCNUM)
      INVAL(2) = RPVAL(2,LOCNUM)
      INPREF(2) = CPREF(2,LOCNUM)
      INTYP(2) = CTYP(2,LOCNUM)
      INSCL = XYSCL(1)
      INOFF = XYOFF(1)
      REVERS = ININC(1).LT.0
C                                       get freq
      CALL H2CHR (8, 1, CATH(KHCTP), AXTYPE)
      IF (ITYP.EQ.2) THEN
         IF (AXTYPE(:4).EQ.'FREQ') THEN
            BVAL = CATD(KDCRV) + (BC - CATR(KRCRP)) * CATR(KRCIC)
            EVAL = CATD(KDCRV) + (EC - CATR(KRCRP)) * CATR(KRCIC)
         ELSE IF ((AXTYPE(2:4).EQ.'ELO') .AND. (CATBLK(KIALT).GT.0)
     *      .AND. (CATD(KDRST).GT.1.E6)) THEN
            REFNU = CATD(KDARV)
            REFAP = CATR(KRARP)
            VSIGN = 1.0D0
            IF (AXTYPE(:1).EQ.'V') VSIGN = -1.0D0
            DELNU = -CATR(KRCIC) * CATD(KDARV) / (VELITE + VSIGN *
     *         CATD(KDCRV) + CATR(KRCIC) * (CATR(KRCRP) - CATR(KRARP)))
            BVAL = REFNU + (BC - REFAP) * DELNU
            EVAL = REFNU + (EC - REFAP) * DELNU
            AXTYPE = 'FREQ'
         ELSE
            MSGTXT = 'CANNOT FIND FREQUENCY - REVERT TO CHANNELS'
            CALL MSGWRT (7)
            ITYP = 1
            END IF
C                                       get velocity
      ELSE IF (ITYP.EQ.3) THEN
         IF (AXTYPE(2:4).EQ.'ELO') THEN
            BVAL = CATD(KDCRV) + (BC - CATR(KRCRP)) * CATR(KRCIC)
            EVAL = CATD(KDCRV) + (EC - CATR(KRCRP)) * CATR(KRCIC)
         ELSE IF ((AXTYPE(:4).EQ.'FREQ') .AND. (CATBLK(KIALT).GT.0)
     *      .AND. (CATD(KDRST).GT.1.E6)) THEN
            REFV = CATD(KDARV)
            REFAP = CATR(KRARP)
            DELNU = CATR(KRCIC)
            REFNU = CATD(KDCRV)
            J = CATBLK(KIALT)/256 + 2
            IF (J.EQ.3) THEN
               VSIGN = -1.0D0
               AXTYPE = 'VELO'
            ELSE
               VSIGN = 1.0D0
               AXTYPE = 'FELO'
               END IF
            DELV = -DELNU * (VELITE + VSIGN*REFV) /
     *         (REFNU + DELNU * (REFAP - CATR(KRCRP)))
            J = MOD (CATBLK(KIALT), 256)
            AXTYPE(5:8) = VELREF(J)
            BVAL = REFV + (BC - REFAP) * DELV
            EVAL = REFV + (EC - REFAP) * DELV
         ELSE
            MSGTXT = 'CANNOT FIND VELOCITY - REVERT TO CHANNELS'
            CALL MSGWRT (7)
            ITYP = 1
            END IF
         END IF
      SPREF = ' '
      STYPE = 'Channels'
      IF ((ITYP.EQ.2) .OR. (ITYP.EQ.3)) THEN
         I = 1
         XYOFF(I) = BVAL
         XYSCL(I) = EVAL
         TEMP = 0.03 * (XYSCL(I) - XYOFF(I))
         XYSCL(I) = XYSCL(I) + TEMP
         XYOFF(I) = XYOFF(I) - TEMP
         XYSCL(I) = 1000.0 / (XYSCL(I) - XYOFF(I))
         TR = 1000.0 / XYSCL(I)
         TI = TR
         IF (REVERS) THEN
            RPLOC(I,LOCNUM) = TRC(I)
            AXINC(I,LOCNUM) = TR / (BLC(I) - TRC(I))
         ELSE
            RPLOC(I,LOCNUM) = BLC(I)
            AXINC(I,LOCNUM) = TR / (TRC(I) - BLC(I))
            END IF
         RPVAL(I,LOCNUM) = XYOFF(I)
         CALL METSCL (LABEL, TR, CPREF(I,LOCNUM), GOOD)
         RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * TR / TI
         AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * TR / TI
         SPREF = CPREF(I,LOCNUM)
         STYPE = AXTYPE
         END IF
C                                       channels
      IF ((.NOT.PLOTI) .OR. (.NOT.PLOTV)) THEN
         CTYP(1,LOCNUM) = STYPE
         CTYP(2,LOCNUM) = ' Jy/beam'
         CALL CLAB1 (BLC, TRC, CHOUT, LABEL, XYRATO, .FALSE., PLBUFF,
     *      IRET)
         IF (IRET.NE.0) GO TO 900
         POLAR = 'Vpol'
         IF (PLOTI) POLAR = 'Ipol'
         CALL GPOS (TRC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 900
         DX = -7.0
         DY = -4.0
         CALL GICHAR (1, 4, 0, DX, DY, POLAR, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       two plots - upper
      ELSE
         XBLC(1) = BLC(1)
         XTRC(1) = TRC(1)
         XBLC(2) = LINT
         XTRC(2) = TRC(2)
         TR = (1000.0 - LINT) / XYSCL(2)
         TI = TR
         RPLOC(2,LOCNUM) = XBLC(2)
         RPVAL(2,LOCNUM) = XYOFF(2)
         AXINC(2,LOCNUM) = TR / (XTRC(2) - XBLC(2))
         CALL METSCL (LABEL, TR, CPREF(2,LOCNUM), GOOD)
         RPVAL(2,LOCNUM) = RPVAL(2,LOCNUM) * TR / TI
         AXINC(2,LOCNUM) = AXINC(2,LOCNUM) * TR / TI
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         CTYP(2,LOCNUM) = ' Jy/beam'
         CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, .FALSE.,
     *      PLBUFF,IRET)
         IF (IRET.NE.0) GO TO 900
         CALL GPOS (XTRC(1), XTRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 900
         DX = -7.0
         DY = -4.0
         POLAR = 'Ipol'
         CALL GICHAR (1, 4, 0, DX, DY, POLAR, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       two plots - lower
         XBLC(1) = BLC(1)
         XTRC(1) = TRC(1)
         XBLC(2) = BLC(2)
         XTRC(2) = LINT
         TR = LINT / XYSCL(3)
         TI = TR
         RPLOC(2,LOCNUM) = XBLC(2)
         RPVAL(2,LOCNUM) = XYOFF(3)
         AXINC(2,LOCNUM) = TR / (XTRC(2) - XBLC(2))
         CALL METSCL (LABEL, TR, CPREF(2,LOCNUM), GOOD)
         RPVAL(2,LOCNUM) = RPVAL(2,LOCNUM) * TR / TI
         AXINC(2,LOCNUM) = AXINC(2,LOCNUM) * TR / TI
         CPREF(1,LOCNUM) = SPREF
         CTYP(1,LOCNUM) = STYPE
         CTYP(2,LOCNUM) = ' Jy/beam'
         CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, .FALSE.,
     *      PLBUFF,IRET)
         IF (IRET.NE.0) GO TO 900
         CALL GPOS (XTRC(1), XTRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 900
         DX = -7.0
         DY = -4.0
         POLAR = 'Vpol'
         CALL GICHAR (1, 4, 0, DX, DY, POLAR, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       restore LOC common
 900  RPLOC(1,LOCNUM) = INLOC(1)
      AXINC(1,LOCNUM) = ININC(1)
      RPVAL(1,LOCNUM) = INVAL(1)
      CPREF(1,LOCNUM) = INPREF(1)
      CTYP(1,LOCNUM) =  INTYP(1)
      RPLOC(2,LOCNUM) = INLOC(2)
      AXINC(2,LOCNUM) = ININC(2)
      RPVAL(2,LOCNUM) = INVAL(2)
      CPREF(2,LOCNUM) = INPREF(2)
      CTYP(2,LOCNUM) =  INTYP(2)
      XYSCL(1) = INSCL
      XYOFF(1) = INOFF
C
 999  RETURN
      END
      SUBROUTINE XG2PLH (NCHAN, NP, DBUFF, VBUFF, IRET)
C-----------------------------------------------------------------------
C   XG2PLH does the OUTEXT printout if requested
C   Inputs:
C      NCHAN   I      Number spectral channels
C      NP      I      Number parameters
C      DBUFF   R(*)   Ipol data
C      VBUFF   R(*)   Vpol data
C   Outputs:
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   NCHAN, NP, IRET
      REAL      DBUFF(NCHAN,*), VBUFF(NCHAN,*)
C
      INCLUDE 'XG2PL.INC'
      INCLUDE 'XGTABLE.INC'
      INTEGER   I, J, MP, TXLUN, TXIND, K, JTRIM, L, ITYP
      DOUBLE PRECISION DAX, DSCALE, REFV, REFP, REFI, REFDI, VSIGN
      LOGICAL   SIMPLE
      REAL      PMAX, PSCALE
      CHARACTER LINE*512, AXIS*8, PRAXIS*8, VELREF(6)*4
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA VELREF /'-LSR','-HEL','-OBS','-OPT','-RAD', ' '/
C-----------------------------------------------------------------------
      IRET = 0
      IF (OUTEXT.EQ.' ') GO TO 999
C                                       open output
      TXLUN = 3
      CALL ZTXOPN ('WRIT', TXLUN, TXIND, OUTEXT, .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT TEXT FILE'
         GO TO 990
         END IF
C                                       Ipol printout
C                                       find max column
      MP = 0
      PMAX = 0.0
      DO 20 I = 1,NCHAN
         DO 10 J = MP+1,NP
            IF (DBUFF(I,J).NE.FBLANK) MP = J
 10         CONTINUE
 20      CONTINUE
      DO 25 I = 1,NUMPX
         DO 24 J = 3,2+3*MAXGAU,3
            IF (PRTGV(J,1,I).NE.FBLANK) THEN
               PMAX =  MAX (PMAX, ABS(PRTGV(J,1,I)))
               PMAX =  MAX (PMAX, ABS(PRTGV(J,2,I)))
               END IF
 24         CONTINUE
 25      CONTINUE
      PSCALE = 1.0
      IF (PMAX.GT.9999.) PSCALE = 0.001
      IF (PMAX.LT.9.99) PSCALE = 1000.0
      IF (PMAX.LT.0.00999) PSCALE = 1000000.0
C                                       header
      WRITE (LINE,2000) 'I', INAME1, INCLS1, INSEQ1
      K = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
      IF (IRET.NE.0) GO TO 980
C                                       area
      IF ((IDX.EQ.0) .AND. (IDY.EQ.0)) THEN
         WRITE (LINE,2001) LX, LY
      ELSE IF (IDX.LT.0) THEN
         WRITE (LINE,2002) IDY, LX, LY
      ELSE
         WRITE (LINE,2003) IX1, IY1, IX2, IY2
         END IF
      K = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
      IF (IRET.NE.0) GO TO 980
      IF (PSCALE.NE.1.0) THEN
         WRITE (LINE,2004) PSCALE
         K = JTRIM (LINE)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
         IF (IRET.NE.0) GO TO 980
         END IF
      LINE = ' '
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:1), IRET)
      IF (IRET.NE.0) GO TO 980
C                                       component values
      WRITE (LINE,2010) ('   Peak    Center     FWHM   ', K = 1,MP-3)
      K = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
      IF (IRET.NE.0) GO TO 980
      WRITE (LINE,2011) ('  Jy/beam  channels channels ', K = 1,MP-3)
      K = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
      IF (IRET.NE.0) GO TO 980
      LINE = ' '
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:1), IRET)
      IF (IRET.NE.0) GO TO 980
C                                       component
      DO 50 I = 1,NUMPX
         WRITE (LINE,2012) PRTPIX(1,I), PRTPIX(2,I), PRTGV(1,1,I),
     *      PRTGV(2,1,I)
         IF (PRTGV(1,1,I).EQ.FBLANK) LINE(11:) =
     *      '   INDEF     INDEF  '
         L = 33
         K = 2
         DO 30 J = 1,MP-3
            IF (PRTGV(K+1,1,I).NE.FBLANK) THEN
               WRITE (LINE(L:),2013) PRTGV(K+1,1,I)*PSCALE
            ELSE
               LINE(L:) = '   INDEF '
               END IF
            L = L + 10
            IF (PRTGV(K+2,1,I).NE.FBLANK) THEN
               WRITE (LINE(L:),2013) PRTGV(K+2,1,I)
            ELSE
               LINE(L:) = '   INDEF '
               END IF
            L = L + 10
            IF (PRTGV(K+3,1,I).NE.FBLANK) THEN
               WRITE (LINE(L:),2014) PRTGV(K+3,1,I)
            ELSE
               LINE(L:) = '  INDEF'
               END IF
            L = L + 9
            K = K + 3
 30         CONTINUE
         CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:L), IRET)
         IF (IRET.NE.0) GO TO 980
C                                       error
         WRITE (LINE,2015) PRTGV(1,2,I), PRTGV(2,2,I)
         IF (PRTGV(1,1,I).EQ.FBLANK) LINE(11:) = ' '
         L = 33
         K = 2
         DO 40 J = 1,MP-3
            IF (PRTGV(K+1,1,I).NE.FBLANK) THEN
               WRITE (LINE(L:),2016) PRTGV(K+1,2,I)*PSCALE
            ELSE
               LINE(L:) = ' '
               END IF
            L = L + 10
            IF (PRTGV(K+2,1,I).NE.FBLANK) THEN
               WRITE (LINE(L:),2016) PRTGV(K+2,2,I)
            ELSE
               LINE(L:) = ' '
               END IF
            L = L + 10
            IF (PRTGV(K+3,1,I).NE.FBLANK) THEN
               WRITE (LINE(L:),2017) PRTGV(K+3,2,I)
            ELSE
               LINE(L:) = ' '
               END IF
            L = L + 9
            K = K + 3
 40         CONTINUE
         IF (LINE.NE.' ') THEN
            CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:L), IRET)
            IF (IRET.NE.0) GO TO 980
            END IF
 50      CONTINUE
C                                       coordinate computation
      DSCALE = 1.0D0
      ITYP = CPARM(4) + 1.1
      REFV = CATD(KDCRV)
      REFP = CATR(KRCRP)
      REFI = CATR(KRCIC)
      SIMPLE = .TRUE.
      CALL H2CHR (8, 1, CATH(KHCTP), AXIS)
      PRAXIS = AXIS
C                                       want frequency label
      IF (ITYP.EQ.2) THEN
         DSCALE = 1.D-6
         IF (AXIS(2:4).EQ.'ELO') THEN
            PRAXIS = '  FREQ'
            REFV = CATD(KDARV)
            REFP = CATR(KRARP)
            VSIGN = 1.0D0
            IF (AXIS(:1).EQ.'V') VSIGN = -1.0D0
            REFI= -CATR(KRCIC) * CATD(KDARV) / (VELITE + VSIGN *
     *         CATD(KDCRV) + CATR(KRCIC) * (CATR(KRCRP) - CATR(KRARP)))
            END IF
C                                       want velocity
      ELSE IF (ITYP.EQ.3) THEN
         DSCALE = 1.D-3
         IF (AXIS(:4).EQ.'FREQ') THEN
            REFV = CATD(KDARV)
            REFP = CATR(KRARP)
            VSIGN = 1.0
            REFI = -REFI * (VELITE + VSIGN*REFV) / (CATD(KDCRV) +
     *         CATR(KRCIC) * (REFP - CATR(KRCRP)))
            SIMPLE = .FALSE.
            REFDI = CATR(KRCIC) / (CATD(KDCRV) +
     *         CATR(KRCIC) * (REFP - CATR(KRCRP)))
            I = CATBLK(KIALT) / 256 + 2
            J = MOD (CATBLK(KIALT), 256)
            IF ((J.LE.0) .OR. (J.GT.6)) J = 6
            IF (I.EQ.3) THEN
               PRAXIS = 'VELO' // VELREF(J)
            ELSE
               PRAXIS = 'FELO' // VELREF(J)
               END IF
         ELSE IF (AXIS(1:1).EQ.'F') THEN
            SIMPLE = .FALSE.
            REFDI =  -CATR(KRCIC) * CATD(KDARV) / (VELITE + VSIGN *
     *         CATD(KDCRV) + CATR(KRCIC) * (CATR(KRCRP) - CATR(KRARP)))
            REFDI = REFDI / (CATD(KDARV) + REFDI *
     *         (CATR(KRCRP)-CATR(KRARP)))
            END IF
         END IF
C                                      spectra title
      LINE = ' '
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:1), IRET)
      IF (IRET.NE.0) GO TO 980
      WRITE (LINE,2020) PRAXIS, ('Component', K, K = 1,MP-3)
      K = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
      IF (IRET.NE.0) GO TO 980
      LINE = ' '
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:1), IRET)
      IF (IRET.NE.0) GO TO 980
C                                       Loop over I
      DO 70 I = 1,NCHAN
         IF (SIMPLE) THEN
            DAX = (REFV + (I - REFP) * REFI) * DSCALE
         ELSE
            DAX = REFV + REFI * (I - REFP) / (1 + (I-REFP) * REFDI)
            DAX = DAX * DSCALE
            END IF
         WRITE (LINE,2030) I, DAX
         K = 18
         DO 60 J = 1,MP
            IF (DBUFF(I,J).NE.FBLANK) THEN
               WRITE (LINE(K:K+10),2031) DBUFF(I,J)
            ELSE
               LINE(K:K+10) = '     INDEF '
               END IF
            K = K + 11
 60         CONTINUE
         K = JTRIM (LINE)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
         IF (IRET.NE.0) GO TO 980
 70      CONTINUE
C                                       Vpol printout
      IF (.NOT.DOVPOL) GO TO 980
C                                       find max column
      MP = 0
      DO 120 I = 1,NCHAN
         DO 110 J = MP+1,NP
            IF (VBUFF(I,J).NE.FBLANK) MP = J
 110        CONTINUE
 120     CONTINUE
C                                       spectra header
      LINE = ' '
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:1), IRET)
      IF (IRET.NE.0) GO TO 980
      WRITE (LINE,2000) 'V', INAME2, INCLS2, INSEQ2
      K = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
      IF (IRET.NE.0) GO TO 980
C                                       area
      IF ((IDX.EQ.0) .AND. (IDY.EQ.0)) THEN
         WRITE (LINE,2001) LX, LY
      ELSE IF (IDX.LT.0) THEN
         WRITE (LINE,2002) IDY, LX, LY
      ELSE
         WRITE (LINE,2003) IX1, IY1, IX2, IY2
         END IF
      K = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
      IF (IRET.NE.0) GO TO 980
C                                       component values
      LINE = ' '
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:1), IRET)
      IF (IRET.NE.0) GO TO 980
      WRITE (LINE,2110) ('     Field      ', K = 1,MP-3)
      K = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
      IF (IRET.NE.0) GO TO 980
      WRITE (LINE,2111) ('    channels    ', K = 1,MP-3)
      K = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
      IF (IRET.NE.0) GO TO 980
      LINE = ' '
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:1), IRET)
      IF (IRET.NE.0) GO TO 980
      DO 150 I = 1,NUMPX
C                                       component
         WRITE (LINE,2112) PRTPIX(1,I), PRTPIX(2,I), PRTZV(1,1,I),
     *      PRTZV(1,2,I)
         IF (PRTGV(1,1,I).EQ.FBLANK) LINE(11:) =
     *      '   INDEF   INDEF    '
         L = 29
         K = 2
         DO 130 J = 1,MAX(1,MP-3)
            IF (PRTZV(K,1,I).NE.FBLANK) THEN
               WRITE (LINE(L:),2113) PRTZV(K,1,I), PRTZV(K,2,I)
            ELSE
               LINE(L:) = '  INDEF (      )'
               END IF
            L = L + 16
            K = K + 1
 130        CONTINUE
         CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:L), IRET)
         IF (IRET.NE.0) GO TO 980
 150     CONTINUE
      LINE = ' '
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:1), IRET)
      IF (IRET.NE.0) GO TO 980
C                                       title
      CALL H2CHR (8, 1, CATH(KHCTP), AXIS)
      WRITE (LINE,2020) AXIS, ('Component', K, K = 1,MP-3)
      K = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
      IF (IRET.NE.0) GO TO 980
      LINE = ' '
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:1), IRET)
      IF (IRET.NE.0) GO TO 980
C                                       Loop over V
      DSCALE = 1.0D0
      IF (AXIS(:4).EQ.'FREQ') DSCALE = 1.0D-6
      IF (AXIS(2:4).EQ.'ELO') DSCALE = 1.0D-3
      DO 170 I = 1,NCHAN
         DAX = (CATD(KDCRV) + (I - CATR(KRCRP)) * CATR(KRCIC)) * DSCALE
         WRITE (LINE,2030) I, DAX
         K = 18
         DO 160 J = 1,MP
            IF (VBUFF(I,J).NE.FBLANK) THEN
               WRITE (LINE(K:K+10),2031) VBUFF(I,J)
            ELSE
               LINE(K:K+10) = '     INDEF '
               END IF
            K = K + 11
 160        CONTINUE
         K = JTRIM (LINE)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:K), IRET)
         IF (IRET.NE.0) GO TO 980
 170     CONTINUE
C                                       close
 980  CALL ZTXCLS (TXLUN, TXIND, I)
      IF (I.NE.0) THEN
         WRITE (MSGTXT,1000) I, 'CLOSE TEXT  FILE'
         CALL MSGWRT (7)
         END IF
      WRITE (MSGTXT,1000) IRET, 'WRITING TEXT FILE'
C
 990  IF (IRET.NE.0) CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XG2PLH ERROR',I4,' ON ',A)
 2000 FORMAT (2X,A,'-polarization data for ',A12,'.',A6,'.',I6)
 2001 FORMAT (6X,'Single pixel X,Y =',2I6)
 2002 FORMAT (6X,'Circle of radius',I3,' pixels centered on X,Y=',2I6)
 2003 FORMAT (6X,'Rectangle X,Y BLC=',2I6,'  TRC=',2I6)
 2004 FORMAT (6X,'Peak values multiplied by',F12.3)
 2010 FORMAT ('   Pixel  ',6X,'Baseline',9X,8A)
 2011 FORMAT ('   X    Y   Jy/beam   Jy/b/chan',8A)
 2012 FORMAT (2I5,2F10.6)
 2013 FORMAT (F8.2)
 2014 FORMAT (F7.2)
 2015 FORMAT (11X,2('(',F8.6,')'))
 2016 FORMAT ('(',F7.3,')')
 2017 FORMAT ('(',F6.3,')')
 2020 FORMAT ('Channel',1X,A8,2X,'Image data','  Model sum',
     *   ' Data-model',8(1X,A9,I1))
 2030 FORMAT (I5,F11.4)
 2031 FORMAT (F11.4)
 2110 FORMAT ('   Pixel  ',7X,'Gain',6X,8A)
 2111 FORMAT ('   X    Y',18X,8A)
 2112 FORMAT (2I5,F9.4,' (',F6.4,')')
 2113 FORMAT (F7.3,' (',F6.3,')')
      END
