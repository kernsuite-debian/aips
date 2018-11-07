LOCAL INCLUDE 'PLOTC.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
C
      CHARACTER NAMEIN*12, CLAIN*6, PRTASK*8, INEXT*2, OPCODE*4,
     *   PFILE*48, CPOL(4)*2, CHIF*8
      HOLLERITH XPRTSK(2), XNAMEI(3), XCLAIN(2), XSOUR(4,30), XSTOK,
     *   XEXT, XOPCOD
      REAL   XSIN, XDISIN, XBCHAN, XECHAN, XNCHAV, XCHINC, XBIF, XEIF,
     *   DO3COL, XVERS, CROWDD, XREF, SOLINT, XTIME(8), XDOTV, XGRCH,
     *   XYRATO
      REAL      BUFF1(UVBFSS), XYSCL(3), XYOFF(3), RPARM(20), XYMIN(3),
     *   XYMAX(3), TRC(2), BLC(2), CHOUT(4), TSTR, TFIN
      INTEGER   SEQIN, DISKIN, LUNI, INDI, NCH, VER, JBUFSZ, CNOIN,
     *   NFRQ, GRCHN, TVCHN, TVCORN(4), CHINC, SBUFF(512), INVERS,
     *   NPARMS, NPOL, NNVIS, NCHAV, DOPLAN, BUFFER(2556), LUNPL, FINDPL
      LOGICAL   DOTV
      COMMON /INPARM/ XPRTSK, XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR,
     *   XSTOK, XBCHAN, XECHAN, XNCHAV, XCHINC, XBIF, XEIF, DO3COL,
     *   XEXT, XVERS, CROWDD, XOPCOD, XREF, SOLINT, XTIME, XDOTV,
     *   XGRCH, XYRATO
      COMMON /CHPARM/ NAMEIN, CLAIN, OPCODE, PFILE, PRTASK, CPOL, INEXT,
     *   CHIF
      COMMON /BUFRS/ BUFF1, SBUFF, RPARM, JBUFSZ, BUFFER
      COMMON /UVPCOM/ XYSCL, XYOFF, XYMIN, XYMAX, DOTV, SEQIN, DISKIN,
     *   LUNI, INDI, NCH, VER, CNOIN, NFRQ, TVCHN, GRCHN, TVCORN, CHINC,
     *   NPARMS, NPOL, NNVIS, NCHAV, DOPLAN, TRC, BLC, LUNPL, FINDPL,
     *   CHOUT, INVERS, TSTR, TFIN
LOCAL END
      PROGRAM PLOTC
C-----------------------------------------------------------------------
C! PLOTC plots colors with labels for a variety of tasks
C# EXT-appl Graphics Plot appl UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2018
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
C   PLOTC plots uv data . A 'PL' extension file is made which can
C   be displayed in the usual ways .
C   Inputs:
C     INNAME         NAMEIN        Name of input UV data.
C     INCLASS        CLAIN         Class of input UV data.
C     INSEQ          SEQIN         Seq. of input UV data.
C     INDISK         DISKIN        Disk number of input VU data.
C     BIF            BIF           IF number to start
C     EIF            EIF           Through IF number
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, TVPLAN(2), NX, NY, NZ, NWORDS, IERR
      LONGINT   PTVPLN
      INCLUDE 'PLOTC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'PLOTC '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL UVPIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       make plane memory: TV
      IF (DOPLAN.LE.2) THEN
         NX = GPHSCX + 1.01
         NY = GPHSCY + 1.01
C                                       plot file
      ELSE
         NX = TRC(1) - BLC(1) + 1.01
         NY = TRC(2) - BLC(2) + 1.01
         END IF
      NZ = 1
      IF (MOD(DOPLAN,2).EQ.0) NZ = 3
      NWORDS = (NX * NY * NZ - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', 'PLPLAN', NWORDS, TVPLAN, PTVPLN, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         GO TO 995
         END IF
      NWORDS = NX * NY * NZ
      CALL FILL (NWORDS, 0, TVPLAN(1+PTVPLN))
C                                       call routine for each task
      IF (PRTASK.EQ.'UVPLT') THEN
         CALL CUVPLT (NX, NY, NZ, TVPLAN(1+PTVPLN), IRET)
      ELSE IF ((PRTASK.EQ.'EDITA') .OR. (PRTASK.EQ.'EDITR') .OR.
     *   (PRTASK.EQ.'SNEDT')) THEN
         CALL CEDITA (NX, NY, NZ, TVPLAN(1+PTVPLN), IRET)
      ELSE IF ((PRTASK.EQ.'SNPLT') .OR. (PRTASK.EQ.'ELFIT')) THEN
         CALL CSNPLT (NX, NY, NZ, TVPLAN(1+PTVPLN), IRET)
      ELSE IF (PRTASK.EQ.'VPLOT') THEN
         CALL CVPLOT (NX, NY, NZ, TVPLAN(1+PTVPLN), IRET)
      ELSE IF (PRTASK.EQ.'ANBPL') THEN
         CALL CANBPL (NX, NY, NZ, TVPLAN(1+PTVPLN), IRET)
      ELSE IF (PRTASK.EQ.'SNIFS') THEN
         CALL CSNIFS (NX, NY, NZ, TVPLAN(1+PTVPLN), IRET)
      ELSE IF ((PRTASK.EQ.'BPEDT') .OR. (PRTASK.EQ.'PCEDT')) THEN
         CALL CBPEDT (NX, NY, NZ, TVPLAN(1+PTVPLN), IRET)
         END IF
C                                       Clear catlg on error
      IF ((IRET.NE.0) .AND. (NCFILE.GE.1) .AND. (FRW(1).LE.0) .AND.
     *   (.NOT.DOTV)) THEN
         CALL DELEXT ('PL', FVOL(1), FCNO(1), 'READ', CATBLK, SBUFF,
     *      VER, IERR)
         CALL ZCLOSE (LUNI, INDI, IERR)
         NCFILE = NCFILE - 1
         END IF
C                                       Close down
 995  CALL DIE (IRET, SBUFF)
C
 999  STOP
      END
      SUBROUTINE UVPIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   UVPIN gets input parameters for PLOTC .
C   Inputs:
C      PRGM   C*6   Program name
C   Output:
C      IRET   I     Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      CHARACTER UTYPE*2, STAT*4, POLS(13)*2
      INTEGER   IUSER, I, IERR, IROUND, LUNTB, LTYPE, IDEPTH(5), ITYPE,
     *   IPSIZE, J
      LOGICAL   T, F
      REAL      DX, DY, CATR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'PLOTC.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (CATD, CATR, CATBLK)
      DATA T, F /.TRUE., .FALSE./
      DATA LUNTB /19/
      DATA IDEPTH /5*1/
      DATA POLS /'YX','XY','YY','XX','LR','RL','LL','RR',' ',' I',' Q',
     *   ' U',' V'/
C-----------------------------------------------------------------------
C                                       Init IO et al.
      TSKNAM = PRGM
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      CHIF = 'IF'
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
      VER = 10000
      NPOL = 0
C                                       Get input parameters.
      NPARMS = 154
      CALL GTPARM (PRGM, NPARMS, RQUICK, XPRTSK, SBUFF, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         RQUICK = .TRUE.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SBUFF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (8, 1, XPRTSK, PRTASK)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (2, 1, XEXT, INEXT)
      IF ((PRTASK.EQ.'SNPLT') .OR. (PRTASK.EQ.'ELFIT') .OR.
     *   (PRTASK.EQ.'SNIFS')) THEN
         DO 25 I = 1,30
            CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 25         CONTINUE
         END IF
      DO3COL = MAX (1.0, DO3COL)
C                                       Crunch input parameters.
      IUSER = NLUSER
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      INVERS = XVERS + 0.1
      TVCHN = 1
C                                       Get CATBLK from file.
      LUNI = 48
      UTYPE = 'UV'
      STAT = 'HDWR'
      IF (DOTV) STAT = 'READ'
      CALL MAPOPN (STAT, DISKIN, NAMEIN, CLAIN, SEQIN, UTYPE, IUSER,
     *   LUNI, INDI, CNOIN, CATBLK, SBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         GO TO 990
         END IF
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      XDISIN = DISKIN
      XSIN = SEQIN
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
      IF (DOTV) FRW(NCFILE) = 0
      XSIN = SEQIN
      XDISIN = DISKIN
      CALL COPY (256, CATBLK, CATUV)
      IF ((PRTASK.EQ.'SNPLT') .OR. (PRTASK.EQ.'ELFIT') .OR.
     *   (PRTASK.EQ.'SNIFS')) THEN
         CALL FNDEXT (INEXT, CATBLK, J)
         IF ((VER.LT.1) .OR. (VER.GT.J)) INVERS = J
         XVERS = INVERS
         END IF
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       time range
      IF ((XTIME(1)+XTIME(2)+XTIME(3)+XTIME(4)) .EQ.0.0)
     *   XTIME(1)=-1.0E6
      IF ((XTIME(5)+XTIME(6)+XTIME(7)+XTIME(8)) .EQ.0.0)
     *   XTIME(5)=1.0E6
      TSTR = XTIME(1) + XTIME(2) / 24. + XTIME(3) / (24. * 60.)
     *   + XTIME(4) / (24. * 60. * 60.)
      TFIN = XTIME(5) + XTIME(6) / 24. + XTIME(7) / (24. * 60.)
     *   + XTIME(8) / (24. * 60. * 60.)
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      IUDISK = DISKIN
      IUCNO = CNOIN
      USEQ = SEQIN
C
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, BCHAN)
      IF (BCHAN.GT.CATBLK(KINAX+JLOCF)) BCHAN = CATBLK(KINAX+JLOCF)
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.GT.CATBLK(KINAX+JLOCF)) ECHAN = CATBLK(KINAX+JLOCF)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      CHINC = IROUND (XCHINC)
      NCHAV = IROUND (XNCHAV)
      NCHAV = MAX (1, MIN (ECHAN-BCHAN+1, NCHAV))
      CHINC = IROUND (XCHINC)
      IF (CHINC.LE.0) CHINC = NCHAV
      IF (NCHAV.GE.ECHAN-BCHAN+1) CHINC = NCHAV
      I = (ECHAN - BCHAN) / CHINC
      ECHAN = BCHAN + I * CHINC
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         BIF = MIN (MAX (1, BIF), CATBLK(KINAX+JLOCIF))
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         END IF
      XBCHAN = BCHAN
      XECHAN = ECHAN
      XBIF = BIF
      XEIF = EIF
      XCHINC = CHINC
      LUNPL = 28
C                                       extension file?
C                                       init plot
C                                       Update catalog header.
      VER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKIN, FCNO(1), CATBLK, SBUFF, T, 'READ',
     *      VER, IRET)
         FRW(NCFILE) = 0
         IF (IRET.NE.0) NCFILE = NCFILE - 1
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       number stokes
      CALL UVGET ('INIT', RPARM, BUFF1, IRET)
      IF (IRET.GT.0) GO TO 999
      NPOL = CATBLK(KINAX+JLOCS)
      DO 30 I = 1,NPOL
         J = CATD(KDCRV+JLOCS) + (I - CATR(KRCRP+JLOCS)) *
     *      CATR(KRCIC+JLOCS) + 9.5
         CPOL(I) = POLS(J)
 30      CONTINUE
      CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
      CALL COPY (256, CATUV, CATBLK)
C                                       Create plot file
      CALL ZPHFIL ('PL', DISKIN, FCNO(1), VER, PFILE, IERR)
      IF (IERR.NE.0) GO TO 999
      IPSIZE = 0
      ITYPE = 52
      CALL GINIT (DISKIN, FCNO(1), PFILE, IPSIZE, ITYPE, NPARMS, XPRTSK,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFFER, LUNPL, FINDPL,
     *   IERR)
      IRET = 2
      IF (IERR.NE.0) GO TO 999
C                                       character surrounding
      CALL RFILL (4, 0.5, CHOUT)
      LTYPE = 3
      CHOUT(1) = 3.5
      CHOUT(2) = 4.0
      CHOUT(4) = CHOUT(4) + 2.833
      IF (PRTASK.EQ.'ELFIT') DO3COL = 2.0
      IF (((PRTASK.EQ.'SNPLT') .OR. (PRTASK.EQ.'ELFIT')) .AND.
     *   (DO3COL.GT.1.5)) THEN
         CHOUT(1) = 17
         CHOUT(2) = 0.5
      ELSE IF (PRTASK.EQ.'SNIFS') THEN
         CHOUT(1) = 7.5
      ELSE IF ((PRTASK.EQ.'BPEDT') .OR. (PRTASK.EQ.'PCEDT')) THEN
         CHOUT(1) = 13
         END IF
C                                       default XYRATIO
      IF (XYRATO.LT.0.01) THEN
         IF (DOTV) THEN
            DX = WINDTV(3) - WINDTV(1) + 1 - CSIZTV(1) * (CHOUT(1)
     *         + CHOUT(3))
            DY = WINDTV(4) - WINDTV(2) + 1 - CSIZTV(2) * (CHOUT(2)
     *         + CHOUT(4))
            XYRATO = 1.0
            IF (DY.GT.0.0) XYRATO = DX / DY
         ELSE
            XYRATO = 1.0
            END IF
         END IF
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      IF ((.NOT.DOTV) .AND. (XYRATO.NE.1.0)) THEN
         IF (XYRATO.GT.1.0) THEN
            I = 1000.0 / XYRATO + 0.5
            TRC(2) = I
         ELSE
            I = 1000.0 * XYRATO + 0.5
            TRC(2) = I
            END IF
         END IF
C                                       plot memory
      DOPLAN = 0
      IF (DOTV) THEN
         DOPLAN = 1
      ELSE
         DOPLAN = 3
         END IF
      IF (DO3COL.GT.0.0) DOPLAN = DOPLAN + 1
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, IDEPTH, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVPIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1035 FORMAT ('ERROR',I3,' FINDING THE UV DATA SET')
      END
      SUBROUTINE PLCOL3 (COLV, ICOL)
C-----------------------------------------------------------------------
C   Color integers returned
C   Input:
C      COLV   R      Color level 0 - 1
C   Output
C      ICOL   I(3)   R, G, B colors (0 to 1) * 32767
C-----------------------------------------------------------------------
      REAL      COLV
      INTEGER   ICOL(3)
C
      INTEGER   I
      REAL      COL(3)
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IF (MAXINT.LE.0) MAXINT = 8191
      CALL COLOR3 (COLV, .FALSE., COL)
      DO 10 I = 1,3
         ICOL(I) = COL(I) * MAXINT + 0.5
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CUVPLT (NX, NY, NZ, TVPLAN, IRET)
C-----------------------------------------------------------------------
C   CUVPLT plots the color scheme used by UVPLT
C   Inputs:
C      NX       I      Number X pixels in TVPLAN
C      NY       I      Number Y pixels in TVPLAN
C      NZ       I      Number Z pixels in TVPLAN
C   In/out
C      TVPLAN   I(*)   Image memory - 0 on input
C   Output
C      IRET     I      Erroro code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), IRET
C
      INTEGER   IX, IY, MX, MY, ICOL(3), JJJ, IIF, IIC, IIP, IY1, IY2,
     *   IX1, IX2, JX, ID(3), IT(3), INCHAR
      REAL      COLV, DCOLV, DX, DY, SX, RX
      CHARACTER TIME*8, DATE*12, TEXT*80
      INCLUDE 'PLOTC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      JJJ = (ECHAN - BCHAN) / CHINC + 1
      JJJ = (EIF - BIF + 1) * JJJ * NPOL
      DCOLV = 0.97 / (JJJ - 1.0)
C                                       sizes of things
      MX = NX / (JJJ / NPOL)
      MY = NY / NPOL
      IF (MX.LT.2) THEN
         MSGTXT = 'TOO MANY CHANNELS FOR GOOD DISPLAY'
         CALL MSGWRT (7)
         MX = 1
         END IF
C                                       do it
      COLV = 0.0
      JX = 1
      DO 100 IIF = BIF,EIF
         DO 90 IIC = BCHAN,ECHAN,CHINC
            IX1 = JX
            IX2 = JX + MAX (0, MX-2)
            JX = JX + MX
            DO 80 IIP = 1,NPOL
               IY1 = 1 + (IIP-1) * MY
               IY2 = IY1 + MY - 2
               IF (NPOL.EQ.1) IY2 = NY
               CALL PLCOL3 (COLV, ICOL)
               COLV = COLV + DCOLV
               DO 30 IY = IY1,IY2
                  DO 20 IX = IX1,IX2
                     TVPLAN(IX,IY,1) = ICOL(1)
                     TVPLAN(IX,IY,2) = ICOL(2)
                     TVPLAN(IX,IY,3) = ICOL(3)
 20                  CONTINUE
 30               CONTINUE
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C                                       draw the array
      IF (DOTV) THEN
         CALL DRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
      ELSE
         CALL GRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
         END IF
      IF (IRET.NE.0) GO TO 975
C                                       Draw border
      CALL GLTYPE (1, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Stokes labels: Y axis
      SX = (TRC(2) - BLC(2)) / NPOL
      SX = SX / 2.0
      RX = SX
      DY = -0.5
      DX = -2.5
      DO 110 IIP = 1,NPOL
         CALL GPOS (BLC(1), SX, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GCHAR (2, 0, DX, DY, CPOL(IIP), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         SX = SX + 2.0*RX
 110     CONTINUE
C                                       Frequency, IF: X axis
      IF (DOTV) THEN
         CALL DRAXIS (NX, MX, IRET)
      ELSE
         CALL GRAXIS (NX, MX, IRET)
         END IF
      IF (IRET.NE.0) GO TO 970
C                                       Top labels: date and time
      DX = 0.0
      DY = CHOUT(4) - 1.5
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, TIME, DATE)
      WRITE (TEXT,1030) VER, DATE, TIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      DY = DY - 1.333
C                                       top: type
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      TEXT = '3-COLOR PLOT FOR TASK ' // PRTASK
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Done: finish plot
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.NE.0) GO TO 975
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1900) VER
         CALL MSGWRT (2)
         END IF
      IRET = 0
      GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 990
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IRET)
         CALL ZDESTR (DISKIN, PFILE, IRET)
         END IF
      GO TO 999
C
 990  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Plot file version',I4,' _created ',A12,A8)
 1900 FORMAT ('Plot file version',I4,' created')
 1970 FORMAT ('CUVPLT: ERROR DURING GRAPHING. WILL TRY TO FINISH',
     *   ' PARTIAL GRAPH')
      END
      SUBROUTINE DRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
C-----------------------------------------------------------------------
C   Draw TVPLAN into memory planes of TV only
C   Inputs:
C      NX       I      X dimension
C      NY       I      Y dimension
C      TVPLAN   I(*)   plane to draw
C   Outputs:
C      BUFFER   I(*)   plot buffer
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), BUFFER(*), IRET
C
      INTEGER   LX, IY, LY, CHAN, LC
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
C                                       3 color grey scale
      DO 50 LC = 1,NZ
         CHAN = GPHTVC(LC)
         CALL GCINIT (CHAN, LC, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       load image
         LX = GPHIX0
         LY = GPHIY0 - 1
         DO 30 IY = 1,NY
            LY = LY + 1
            CALL YIMGIO ('WRIT', CHAN, LX, LY, 0, NX,
     *         TVPLAN(1,IY,LC), IRET)
            IF (IRET.NE.0) GO TO 999
 30         CONTINUE
 50      CONTINUE

 999  RETURN
      END
      SUBROUTINE GRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
C-----------------------------------------------------------------------
C   Draw TVPLAN into a plot file
C   Inputs:
C      NX       I      X dimension
C      NY       I      Y dimension
C      NZ       I      Z dimension (1 B&W, 3 color)
C      TVPLAN   I(*)   plane to draw
C   Outputs:
C      BUFFER   I(*)   plot buffer
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), BUFFER(*), IRET
C
      INTEGER   LX, IY, LY, IGLO, IGHI
      REAL      RANGES(2,3), X, Y
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IF (MAXINT.LE.0) MAXINT = 8191
C                                       3 color grey scale
      IGLO = 0
      IGHI = MAXINT
      RANGES(1,1) = 0.0
      RANGES(2,1) = MAXINT
      RANGES(1,2) = 0.0
      RANGES(2,2) = MAXINT
      RANGES(1,3) = 0.0
      RANGES(2,3) = MAXINT
      CALL GINITC (IGLO, IGHI, RANGES, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       load image
      LX = GPHIX1
      X = LX
      LY = GPHIY1 - 1
      DO 20 IY = 1,NY
         LY = LY + 1
         Y = LY
         CALL GPOS (X, Y, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL G3COLR (NX, 0, TVPLAN(1,IY,1), TVPLAN(1,IY,2),
     *      TVPLAN(1,IY,3), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 999
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE DRAXIS (NX, MX, IRET)
C-----------------------------------------------------------------------
C   DRAXIS plots the X axis labels for a FREQ, IF axis on the TV
C   Inputs:
C      NX     I   Number points on X axis
C      MX     I   Number points per channel incl a blank
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   NX, MX, IRET
C
      INCLUDE 'PLOTC.INC'
      INTEGER   IIF, NC, IX(2), IY(2), CHAN, J, LX, LY, IANGL, IG1, IC,
     *   IIC, XX(2), DT, DD, JTRIM, JC, LEIF
      CHARACTER CIF*8
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NC = (ECHAN - BCHAN) / CHINC + 1
      CHAN = GPHTVG(GPHLTY)
      IG1 = CHAN - NGRAY
C                                       IF values and boundaries
      IF ((ECHAN.GT.BCHAN) .AND. (EIF.GE.BIF)) THEN
         IY(1) = GPHIY0 - 1.5 * CSIZTV(2)
         IY(2) = IY(1) - 2.0 * CSIZTV(2)
      ELSE
         IY(1) = GPHIY0 - 0.2 * CSIZTV(2)
         IY(2) = IY(1) - 1.8 * CSIZTV(2)
         END IF
      IX(1) = GPHIX0
      IX(2) = IX(1)
      IF (EIF.GE.BIF) THEN
         CALL IMVECT ('ONNN', CHAN, 2, IX, IY, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      JC = JTRIM (CHIF)
      LY = (1.2*IY(2) + 0.8*IY(1) - CSIZTV(2)) / 2.0
      LX = IX(1) - (JC+0.5) * CSIZTV(1)
      IANGL = 0
      CALL IMANOT ('WRIT', IG1, LX, LY, IANGL, 0, CHIF(:JC), BUFFER,
     *   IRET)
      IF (IRET.NE.0) GO TO 990
      IF (EIF.GE.BIF) THEN
         IX(1) = IX(1) - 1
         DO 30 IIF = BIF,EIF
            WRITE (CIF,1000) IIF
            CALL CHTRIM (CIF, 8, CIF, J)
            LX = IX(1) + (MX * NC - J * CSIZTV(1)) / 2
            CALL IMANOT ('WRIT', IG1, LX, LY, IANGL, 0, CIF(:J), BUFFER,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            IX(1) = IX(1) + NC * MX
            IX(2) = IX(1)
            CALL IMVECT ('ONNN', CHAN, 2, IX, IY, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 990
 30         CONTINUE
         END IF
C                                       channels
      IF (BCHAN.GE.ECHAN) GO TO 999
      WRITE (CIF,1000) ECHAN
      CALL CHTRIM (CIF, 8, CIF, J)
      LEIF = MAX (BIF, EIF)
C                                       label each one
      IY(1) = GPHIY0
      IY(2) = IY(1) + 1.5*CSIZTV(2)
      IF (J*CSIZTV(1).LT.MX-1) THEN
         XX(1) = GPHIX0
         DO 130 IIF = BIF,LEIF
            XX(2) = XX(1)
            IC = BCHAN + (CHINC - 1) / 2
            LY = GPHIY0 - 1.5*CSIZTV(2)
            XX(1) = XX(1) + NC * MX
            DO 120 IIC = BCHAN,ECHAN,CHINC
               WRITE (CIF,1000) IC
               CALL CHTRIM (CIF, 8, CIF, J)
               LX = XX(2) + (MX - J * CSIZTV(1)) / 2
               CALL IMANOT ('WRIT', IG1, LX, LY, IANGL, 0, CIF(:J),
     *            BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               IX(1) = XX(2) + MX / 2
               IX(2) = IX(1)
               CALL IMVECT ('ONNN', CHAN, 2, IX, IY, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               XX(2) = XX(2) + MX
               IC = IC + CHINC
 120           CONTINUE
 130        CONTINUE
C                                       label every other one
      ELSE IF (J*CSIZTV(1).LT.2*MX-1) THEN
         XX(1) = GPHIX0 + MX + (MX-1) / 2
         DO 150 IIF = BIF,LEIF
            XX(2) = XX(1)
            IC = BCHAN + CHINC
            LY = GPHIY0 - 1.5*CSIZTV(2)
            XX(1) = XX(1) + NC * MX
            DO 140 IIC = BCHAN,ECHAN,2*CHINC
               IF (IC.GT.ECHAN) GO TO 150
               WRITE (CIF,1000) IC
               CALL CHTRIM (CIF, 8, CIF, J)
               LX = XX(2) - (J * CSIZTV(1)) / 2
               CALL IMANOT ('WRIT', IG1, LX, LY, IANGL, 0, CIF(:J),
     *            BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               IX(1) = XX(2)
               IX(2) = IX(1)
               CALL IMVECT ('ONNN', CHAN, 2, IX, IY, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               XX(2) = XX(2) + 2*MX
               IC = IC + 2*CHINC
 140           CONTINUE
 150        CONTINUE
C                                       standard ticks
      ELSE
         IC = ECHAN - BCHAN
         IF (IC.GT.600) THEN
            DT = 200
         ELSE IF (IC.GT.300) THEN
            DT = 100
         ELSE IF (IC.GT.60) THEN
            DT = 20
         ELSE
            DT = 10
            END IF
         XX(1) = GPHIX0
         XX(2) = XX(1) + NC * MX
         XX(1) = GPHIX0 + (MX-1)/2
         XX(2) = XX(1) + (NC-1) * MX
         LY = GPHIY0 - 1.5*CSIZTV(2)
         DO 170 IIF = BIF,LEIF
            DD = (BCHAN / DT) * DT
            IF (DD.LT.BCHAN) DD = DD + DT
 160        IF (DD.LT.ECHAN) THEN
               WRITE (CIF,1000) DD
               CALL CHTRIM (CIF, 8, CIF, J)
               LX = FLOAT (DD - BCHAN) / FLOAT (ECHAN - BCHAN) *
     *            (XX(2) - XX(1)) + 0.5 + XX(1)
               IX(1) = LX
               IX(2) = LX
               LX = LX - (J * CSIZTV(1))/2
               CALL IMVECT ('ONNN', CHAN, 2, IX, IY, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               CALL IMANOT ('WRIT', IG1, LX, LY, IANGL, 0, CIF(:J),
     *            BUFFER, IRET)
               DD = DD + DT
               GO TO 160
               END IF
            XX(1) = XX(1) + NC * MX
            XX(2) = XX(2) + NC * MX
 170        CONTINUE
         END IF
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I8)
 1990 FORMAT ('DRAXIS: TV ERROR',I5,' PLOTTING X AXIS')
      END
      SUBROUTINE GRAXIS (NX, MX, IRET)
C-----------------------------------------------------------------------
C   GRAXIS plots the X axis labels for a FREQ, IF axis in a plot file
C   Inputs:
C      NX     I   Number points on X axis
C      MX     I   Number points per channel incl a blank
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   NX, MX, IRET
C
      INCLUDE 'PLOTC.INC'
      INTEGER   IIF, NC, J, IC, IIC, XX(2), DT, DD, JC, JTRIM, LEIF
      CHARACTER CIF*8
      REAL      DX, DY, RX(2), RY(2), LX, LY
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NC = (ECHAN - BCHAN) / CHINC + 1
C                                       IF values and boundaries
      IF ((ECHAN.GT.BCHAN) .AND. (EIF.GE.BIF)) THEN
         RY(1) = GPHIY1 - 1.5 * 9
         RY(2) = RY(1) - 2.0 * 9
      ELSE
         RY(1) = GPHIY1 - 0.2 * 9
         RY(2) = RY(1) - 1.8 * 9
         END IF
      RX(1) = GPHIX1
      RX(2) = RX(1)
      IF (EIF.GE.BIF) THEN
         CALL GPOS (RX(1), RY(1), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL GVEC (RX(2), RY(2), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      DX = 0.0
      DY = 0.0
      LY = GPHIY1
      LX = RX(1)
      JC = JTRIM (CHIF)
      DX = -1.5
      DY = -JC - 0.5
      IF (BCHAN.GE.ECHAN) DY = -1.6
      CALL GPOS (LX, LY, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL GCHAR (2, 0, DX, DY, CHIF(:JC), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (EIF.GE.BIF) THEN
         RX(1) = RX(1) - 1
         DO 30 IIF = BIF,EIF
            WRITE (CIF,1000) IIF
            CALL CHTRIM (CIF, 8, CIF, J)
            LX = RX(1) + (MX * NC) / 2
            DX = -J/2.0
            CALL GPOS (LX, LY, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL GCHAR (J, 0, DX, DY, CIF(:J), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 990
            RX(1) = RX(1) + NC * MX
            RX(2) = RX(1)
            CALL GPOS (RX(1), RY(1), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL GVEC (RX(2), RY(2), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 990
 30         CONTINUE
         END IF
C                                       channels
      IF (BCHAN.GE.ECHAN) GO TO 999
      WRITE (CIF,1000) ECHAN
      CALL CHTRIM (CIF, 8, CIF, J)
      LEIF = MAX (EIF, BIF)
C                                       label each one
      RY(1) = GPHIY1
      RY(2) = RY(1) + 1.5*9
      IF (J*7.LT.MX-1) THEN
         XX(1) = GPHIX1
         DO 130 IIF = BIF,LEIF
            XX(2) = XX(1)
            IC = BCHAN + (CHINC - 1) / 2
            LY = GPHIY1
            DY = -1.5
            XX(1) = XX(1) + NC * MX
            DO 120 IIC = BCHAN,ECHAN,CHINC
               WRITE (CIF,1000) IC
               CALL CHTRIM (CIF, 8, CIF, J)
               LX = XX(2) + MX/2
               DX = -J/2.0
               CALL GPOS (LX, LY, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               CALL GCHAR (J, 0, DX, DY, CIF(:J), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               RX(1) = XX(2) + MX / 2
               RX(2) = RX(1)
               CALL GPOS (RX(1), RY(1), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               CALL GVEC (RX(2), RY(2), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               XX(2) = XX(2) + MX
               IC = IC + CHINC
 120           CONTINUE
 130        CONTINUE
C                                       label every other one
      ELSE IF (J*7.LT.2*MX-1) THEN
         XX(1) = GPHIX1 + MX + (MX-1)/2
         DO 150 IIF = BIF,LEIF
            XX(2) = XX(1)
            IC = BCHAN + CHINC
            LY = GPHIY1
            DY = -1.5
            XX(1) = XX(1) + NC * MX
            DO 140 IIC = BCHAN,ECHAN,2*CHINC
               IF (IC.GT.ECHAN) GO TO 150
               WRITE (CIF,1000) IC
               CALL CHTRIM (CIF, 8, CIF, J)
               LX = XX(2)
               DX = -J/2.0
               CALL GPOS (LX, LY, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               CALL GCHAR (J, 0, DX, DY, CIF(:J), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               RX(1) = XX(2)
               RX(2) = RX(1)
               CALL GPOS (RX(1), RY(1), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               CALL GVEC (RX(2), RY(2), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               XX(2) = XX(2) + 2*MX
               IC = IC + 2*CHINC
 140           CONTINUE
 150        CONTINUE
C                                       standard ticks
      ELSE
         IC = ECHAN - BCHAN
         IF (IC.GT.600) THEN
            DT = 200
         ELSE IF (IC.GT.300) THEN
            DT = 100
         ELSE IF (IC.GT.60) THEN
            DT = 20
         ELSE
            DT = 10
            END IF
         XX(1) = GPHIX1 + MX/2 - 1
         XX(2) = XX(1) + (NC-1) * MX
         LY = GPHIY1
         DY = -1.5
         DO 170 IIF = BIF,LEIF
            DD = (BCHAN / DT) * DT
            IF (DD.LT.BCHAN) DD = DD + DT
 160        IF (DD.LT.ECHAN) THEN
               WRITE (CIF,1000) DD
               CALL CHTRIM (CIF, 8, CIF, J)
               LX = FLOAT (DD - BCHAN) / FLOAT (ECHAN - BCHAN) *
     *            (XX(2) - XX(1)) + XX(1)
               RX(1) = LX
               RX(2) = LX
               DX = -J/2.0
               CALL GPOS (RX(1), RY(1), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               CALL GVEC (RX(2), RY(2), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               CALL GPOS (LX, LY, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               CALL GCHAR (J, 0, DX, DY, CIF(:J), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 990
               DD = DD + DT
               GO TO 160
               END IF
            XX(1) = XX(1) + NC * MX
            XX(2) = XX(2) + NC * MX
 170        CONTINUE
         END IF
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I8)
 1990 FORMAT ('GRAXIS: PLOT ERROR',I5,' PLOTTING X AXIS')
      END
      SUBROUTINE CEDITA (NX, NY, NZ, TVPLAN, IRET)
C-----------------------------------------------------------------------
C   CEDITA plots the color scheme used by EDITA, EDITR, SNEDT
C   Inputs:
C      NX       I      Number X pixels in TVPLAN
C      NY       I      Number Y pixels in TVPLAN
C      NZ       I      Number Z pixels in TVPLAN
C   In/out
C      TVPLAN   I(*)   Image memory - 0 on input
C   Output
C      IRET     I      Erroro code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), IRET
C
      INTEGER   IX, IY, MX, MY, ICOL(3), JJJ, IIF, IIP, IY1, IY2, IX1,
     *   IX2, JX, ID(3), IT(3), INCHAR
      REAL      COLV, DCOLV, DX, DY, SX, RX
      CHARACTER TIME*8, DATE*12, TEXT*80
      INCLUDE 'PLOTC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      BCHAN = 0
      ECHAN = 0
      CHINC = 1
      NPOL = MAX (1, MIN (2, NPOL))
      JJJ = (EIF - BIF + 1) * NPOL
      DCOLV = 0.97 / MAX (1, JJJ-1)
C                                       sizes of things
      MX = NX / (JJJ / NPOL)
      MY = NY / NPOL
      IF (MX.LT.2) THEN
         MSGTXT = 'TOO MANY IFS FOR GOOD DISPLAY'
         CALL MSGWRT (7)
         MX = 1
         END IF
C                                       do it
      COLV = 0.0
      JX = 1
      DO 100 IIF = BIF,EIF
         IX1 = JX
         IX2 = JX + MAX (0, MX-2)
         JX = JX + MX
         DO 80 IIP = 1,NPOL
            IY1 = 1 + (IIP-1) * MY
            IY2 = IY1 + MY - 2
            IF (NPOL.EQ.1) IY2 = NY
            CALL PLCOL3 (COLV, ICOL)
            COLV = COLV + DCOLV
            DO 30 IY = IY1,IY2
               DO 20 IX = IX1,IX2
                  TVPLAN(IX,IY,1) = ICOL(1)
                  TVPLAN(IX,IY,2) = ICOL(2)
                  TVPLAN(IX,IY,3) = ICOL(3)
 20               CONTINUE
 30            CONTINUE
 80         CONTINUE
 100     CONTINUE
C                                       draw the array
      IF (DOTV) THEN
         CALL DRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
      ELSE
         CALL GRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
         END IF
      IF (IRET.NE.0) GO TO 975
C                                       Draw border
      CALL GLTYPE (1, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Stokes labels: Y axis
      SX = (TRC(2) - BLC(2)) / NPOL
      SX = SX / 2.0
      RX = SX
      DY = -0.5
      DX = -2.5
      DO 110 IIP = 1,NPOL
         CALL GPOS (BLC(1), SX, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GCHAR (2, 0, DX, DY, CPOL(IIP), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         SX = SX + 2.0*RX
 110     CONTINUE
C                                       Frequency, IF: X axis
      IF (DOTV) THEN
         CALL DRAXIS (NX, MX, IRET)
      ELSE
         CALL GRAXIS (NX, MX, IRET)
         END IF
      IF (IRET.NE.0) GO TO 970
C                                       Top labels: date and time
      DX = 0.0
      DY = CHOUT(4) - 1.5
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, TIME, DATE)
      WRITE (TEXT,1030) VER, DATE, TIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      DY = DY - 1.333
C                                       top: type
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      TEXT = '3-COLOR PLOT FOR TASK ' // PRTASK
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Done: finish plot
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.NE.0) GO TO 975
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1900) VER
         CALL MSGWRT (2)
         END IF
      IRET = 0
      GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 990
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IRET)
         CALL ZDESTR (DISKIN, PFILE, IRET)
         END IF
      GO TO 999
C
 990  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Plot file version',I4,' _created ',A12,A8)
 1900 FORMAT ('Plot file version',I4,' created')
 1970 FORMAT ('CEDITA: ERROR DURING GRAPHING. WILL TRY TO FINISH',
     *   ' PARTIAL GRAPH')
      END
      SUBROUTINE CSNPLT (NX, NY, NZ, TVPLAN, IRET)
C-----------------------------------------------------------------------
C   CEDITA plots the color scheme used by SNPLT
C   Inputs:
C      NX       I      Number X pixels in TVPLAN
C      NY       I      Number Y pixels in TVPLAN
C      NZ       I      Number Z pixels in TVPLAN
C   In/out
C      TVPLAN   I(*)   Image memory - 0 on input
C   Output
C      IRET     I      Erroro code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), IRET
C
      INCLUDE 'PLOTC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INTEGER   IX, IY, MY, ICOL(3), JJJ, IY1, IY2, IX1,
     *   IX2, ID(3), IT(3), INCHAR, SOUKOL, NKEY, NREC, NCOL, I,
     *   DATP(128,2), NROWS, GNRECI(XCLRSZ), SID(500), NID, NSOUR,
     *   SCNT(5000), STRANS(5000), ISLUN, ISOU, JTRIM, NSOU, IQUAL(30)
      REAL      COLV, DCOLV, DX, DY, SX, RX, GNREC(XCLRSZ), TEMP
      CHARACTER TIME*8, DATE*12, TEXT*80, COLTAB*24, SNAMES(5000)*16
      EQUIVALENCE (GNREC, GNRECI)
      DATA COLTAB /'SOURCE ID'/
      DATA ISLUN /25/
C-----------------------------------------------------------------------
C                                       same scheme as EDITA when IF/pol
      IF ((DO3COL.LT.1.5) .AND. (PRTASK.NE.'ELFIT')) THEN
         CALL CEDITA (NX, NY, NZ, TVPLAN, IRET)
         GO TO 999
         END IF
C                                       source = color, must read table
      CALL FNDEXT (INEXT, CATBLK, JJJ)
      IF (JJJ.LE.0) THEN
         MSGTXT = 'NO TABLES OF TYPE ' // INEXT // ' FOUND'
         IRET = 9
         GO TO 980
         END IF
      IF (INVERS.LE.0) INVERS = JJJ
      IF (INVERS.GT.JJJ) INVERS = JJJ
C                                       Look up sources
      NID = 500
      NSOUR = 30
      MSGSUP = 32000
      CALL FILL (NSOUR, -1, IQUAL)
      CALL SOURNU (SOURCS, IQUAL, NSOUR, DISKIN, CNOIN, NID, BUFF1, SID,
     *   IRET)
      MSGSUP = 0
      IF (IRET.LT.0) THEN
         MSGTXT = 'SPECIFIED SOURCE(S) NOT FOUND - CONTINUING'
         CALL MSGWRT (6)
         END IF
      IF (IRET.NE.0) NID = 0
C                                       Open table
      ICLUN = 28
      NKEY = 0
      NREC = 0
      NCOL = 0
      ICLRNO = 1
      CALL TABINI ('READ', INEXT, DISKIN, CNOIN, INVERS, CATBLK, ICLUN,
     *   NKEY, NREC, NCOL, DATP, CLBUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1001) IRET, INEXT, INVERS
         GO TO 980
         END IF
C                                       Get number of records
      NCLINR = CLBUFF(5)
      NROWS = NCLINR
      CALL FNDCOL (1, COLTAB, 24, .TRUE., CLBUFF, SOUKOL, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.10)) THEN
         MSGTXT = 'SOURCE ID COLUMN NOT FOUND'
         GO TO 980
         END IF
      SOUKOL = DATP(SOUKOL,1)
      CALL FILL (5000, 0, SCNT)
C                                       get source numbers used
      DO 100 ICLRNO = 1,NCLINR
         CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IRET)
         IF (IRET.LT.0) GO TO 100
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING TABLE'
            GO TO 990
            END IF
         ISOU = GNRECI(SOUKOL)
C                                       source selection
         IF (NID.GT.0) THEN
            DO 10 I = 1,NID
               IF (ISOU.EQ.SID(I)) GO TO 20
 10            CONTINUE
            GO TO 100
            END IF
 20      IF ((ISOU.GT.0) .AND. (ISOU.LE.5000)) SCNT(ISOU) = SCNT(ISOU)+1
 100     CONTINUE
      CALL TABIO ('CLOS', 0, ICLRNO, GNREC, CLBUFF, IRET)
C                                       source number translation
      NSOU = 0
      CALL FILL (5000, 0, STRANS)
      DO 110 I = 1,5000
         IF (SCNT(I).GT.0) THEN
            NSOU = NSOU + 1
            STRANS(NSOU) = I
            END IF
 110     CONTINUE
      IF (NSOU.LE.0) THEN
         MSGTXT = 'NO SOURCES FOUND: CHECK INEXT, SOURCES'
         IRET = 8
         GO TO 980
         END IF
C                                       source names
      DO 120 I = 1,NSOU
         MSGSUP = 32000
         ISOU = STRANS(I)
         CALL GETSOU (ISOU, DISKIN, CNOIN, CATBLK, ISLUN, IRET)
         MSGSUP = 0
         JJJ = JTRIM (SNAME)
         SNAMES(I) = ' '
         IF (JJJ.GT.0) SNAMES(I) = SNAME
 120     CONTINUE
      DCOLV = 0.97 / MAX (1, NSOU-1)
      IRET = 0
C                                       sizes of things
      MY = NY / NSOU
C                                       do it
      COLV = 0.0
      IX1 = 1
      IX2 = NX
      DO 150 ISOU = 1,NSOU
         IY1 = 1 + (ISOU-1) * MY
         IY2 = IY1 + MY - 2
         CALL PLCOL3 (COLV, ICOL)
         COLV = COLV + DCOLV
         DO 140 IY = IY1,IY2
            DO 130 IX = IX1,IX2
               TVPLAN(IX,IY,1) = ICOL(1)
               TVPLAN(IX,IY,2) = ICOL(2)
               TVPLAN(IX,IY,3) = ICOL(3)
 130           CONTINUE
 140        CONTINUE
 150     CONTINUE
C                                       draw the array
      IF (DOTV) THEN
         CALL DRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
      ELSE
         CALL GRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
         END IF
      IF (IRET.NE.0) GO TO 975
C                                       Draw border
      CALL GLTYPE (1, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Stokes labels: Y axis
      TEMP = NSOU * MY
      TEMP = TEMP / NY
      SX = TEMP * (TRC(2) - BLC(2)) / NSOU
      SX = SX / 2.0
      RX = SX
      DY = -0.5
      DX = -2.5
      DO 170 ISOU = 1,NSOU
         CALL GPOS (BLC(1), SX, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         JJJ = JTRIM (SNAMES(ISOU))
         DX = -JJJ - 0.5
         CALL GCHAR (JJJ, 0, DX, DY, SNAMES(ISOU)(:JJJ), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         SX = SX + 2.0*RX
 170     CONTINUE
C                                       Top labels: date and time
      DX = 0.0
      DY = CHOUT(4) - 1.5
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, TIME, DATE)
      WRITE (TEXT,1030) VER, DATE, TIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      DY = DY - 1.333
C                                       top: type
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      TEXT = '3-COLOR PLOT FOR TASK ' // PRTASK
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Done: finish plot
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.NE.0) GO TO 975
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1900) VER
         CALL MSGWRT (2)
         END IF
      IRET = 0
      GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 990
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IRET)
         CALL ZDESTR (DISKIN, PFILE, IRET)
         END IF
      GO TO 999
C
 980  CALL MSGWRT (8)
      GO TO 970
C
 990  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CSNPLT ERROR',I4,' ON ',A)
 1001 FORMAT ('CSNPLT ERROR ',I3,' OPENING ',A,' TABLE NO. ',I3)
 1030 FORMAT ('Plot file version',I4,' _created ',A12,A8)
 1900 FORMAT ('Plot file version',I4,' created')
 1970 FORMAT ('CSNPLT: ERROR DURING GRAPHING. WILL TRY TO FINISH',
     *   ' PARTIAL GRAPH')
      END
      SUBROUTINE CVPLOT (NX, NY, NZ, TVPLAN, IRET)
C-----------------------------------------------------------------------
C   CVPLOT plots the color scheme used by VPLOT
C   Inputs:
C      NX       I      Number X pixels in TVPLAN
C      NY       I      Number Y pixels in TVPLAN
C      NZ       I      Number Z pixels in TVPLAN
C   In/out
C      TVPLAN   I(*)   Image memory - 0 on input
C   Output
C      IRET     I      Erroro code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), IRET
C
      INTEGER   IX, IY, MX, MY, ICOL(3), JJJ, IIF, IIC, IIP, IY1, IY2,
     *   IX1, IX2, JX, ID(3), IT(3), INCHAR, IROUND
      REAL      COLV, DCOLV, DX, DY, SX, RX
      CHARACTER TIME*8, DATE*12, TEXT*80
      INCLUDE 'PLOTC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
C                                       interpret CROWDED
      JJJ = IROUND (CROWDD)
      IF (JJJ.EQ.1) THEN
         EIF = 0
         BIF = 0
      ELSE IF (JJJ.EQ.2) THEN
         BCHAN = 0
         ECHAN = 0
      ELSE IF (JJJ.NE.3) THEN
         EIF = 0
         BIF = 0
         ECHAN = 0
         BCHAN = 0
         END IF
      JJJ = (ECHAN - BCHAN) / CHINC + 1
      JJJ = (EIF - BIF + 1) * JJJ * NPOL
      IF (JJJ.LE.1) THEN
         IRET = 1
         WRITE (MSGTXT,1000) CROWDD
         CALL MSGWRT (8)
         GO TO 999
         END IF
      DCOLV = 0.97 / (JJJ - 1.0)
C                                       sizes of things
      MX = NX / (JJJ / NPOL)
      MY = NY / NPOL
      IF (MX.LT.2) THEN
         MSGTXT = 'TOO MANY CHANNELS FOR GOOD DISPLAY'
         CALL MSGWRT (7)
         MX = 1
         END IF
C                                       do it
      COLV = 0.0
      JX = 1
      DO 100 IIF = BIF,EIF
         DO 90 IIC = BCHAN,ECHAN,CHINC
            IX1 = JX
            IX2 = JX + MAX (0, MX-2)
            JX = JX + MX
            DO 80 IIP = 1,NPOL
               IY1 = 1 + (IIP-1) * MY
               IY2 = IY1 + MY - 2
               IF (NPOL.EQ.1) IY2 = NY
               CALL PLCOL3 (COLV, ICOL)
               COLV = COLV + DCOLV
               DO 30 IY = IY1,IY2
                  DO 20 IX = IX1,IX2
                     TVPLAN(IX,IY,1) = ICOL(1)
                     TVPLAN(IX,IY,2) = ICOL(2)
                     TVPLAN(IX,IY,3) = ICOL(3)
 20                  CONTINUE
 30               CONTINUE
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C                                       draw the array
      IF (DOTV) THEN
         CALL DRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
      ELSE
         CALL GRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
         END IF
      IF (IRET.NE.0) GO TO 975
C                                       Draw border
      CALL GLTYPE (1, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Stokes labels: Y axis
      SX = (TRC(2) - BLC(2)) / NPOL
      SX = SX / 2.0
      RX = SX
      DY = -0.5
      DX = -2.5
      DO 110 IIP = 1,NPOL
         CALL GPOS (BLC(1), SX, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GCHAR (2, 0, DX, DY, CPOL(IIP), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         SX = SX + 2.0*RX
 110     CONTINUE
C                                       Frequency, IF: X axis
      IF (DOTV) THEN
         CALL DRAXIS (NX, MX, IRET)
      ELSE
         CALL GRAXIS (NX, MX, IRET)
         END IF
      IF (IRET.NE.0) GO TO 970
C                                       Top labels: date and time
      DX = 0.0
      DY = CHOUT(4) - 1.5
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, TIME, DATE)
      WRITE (TEXT,1030) VER, DATE, TIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      DY = DY - 1.333
C                                       top: type
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      WRITE (TEXT, 1100) PRTASK, CROWDD
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Done: finish plot
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.NE.0) GO TO 975
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1900) VER
         CALL MSGWRT (2)
         END IF
      IRET = 0
      GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 990
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IRET)
         CALL ZDESTR (DISKIN, PFILE, IRET)
         END IF
      GO TO 999
C
 990  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COLOR NOT USED WITH NPOL=1 AND CROWDED=',F5.1)
 1030 FORMAT ('Plot file version',I4,' _created ',A12,A8)
 1100 FORMAT ('3-COLOR PLOT FOR TASK ',A6,' CROWDED =',F5.1)
 1900 FORMAT ('Plot file version',I4,' created')
 1970 FORMAT ('CVPLOT: ERROR DURING GRAPHING. WILL TRY TO FINISH',
     *   ' PARTIAL GRAPH')
      END
      SUBROUTINE CANBPL (NX, NY, NZ, TVPLAN, IRET)
C-----------------------------------------------------------------------
C   CANBPL plots the color scheme used by ANBPL
C   Inputs:
C      NX       I      Number X pixels in TVPLAN
C      NY       I      Number Y pixels in TVPLAN
C      NZ       I      Number Z pixels in TVPLAN
C   In/out
C      TVPLAN   I(*)   Image memory - 0 on input
C   Output
C      IRET     I      Erroro code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), IRET
C
      INTEGER   IX, IY, MX, MY, ICOL(3), JJJ, IIF, IIP, IY1, IY2, IX1,
     *   IX2, JX, ID(3), IT(3), INCHAR
      REAL      COLV, DCOLV, DX, DY, SX, RX
      CHARACTER TIME*8, DATE*12, TEXT*80
      INCLUDE 'PLOTC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
C                                       interpret OPCODE
      BCHAN = 0
      ECHAN = 0
      IF (OPCODE.EQ.'ALST') THEN
         EIF = 0
         BIF = 0
      ELSE IF (OPCODE.EQ.'ALIF') THEN
         NPOL = 1
      ELSE IF (OPCODE.NE.'ALSI') THEN
         MSGTXT = 'ASSUMING OPCODE = ''ALSI'''
         CALL MSGWRT (2)
         OPCODE = 'ALSI'
         END IF
      JJJ = (EIF - BIF + 1) * NPOL
      IF (JJJ.LE.1) THEN
         IRET = 1
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         GO TO 999
         END IF
      DCOLV = 0.97 / (JJJ - 1.0)
C                                       sizes of things
      MX = NX / (JJJ / NPOL)
      MY = NY / NPOL
      IF (MX.LT.2) THEN
         MSGTXT = 'TOO MANY CHANNELS FOR GOOD DISPLAY'
         CALL MSGWRT (7)
         MX = 1
         END IF
C                                       do it
      COLV = 0.0
      JX = 1
      DO 100 IIF = BIF,EIF
         IX1 = JX
         IX2 = JX + MAX (0, MX-2)
         JX = JX + MX
         DO 80 IIP = 1,NPOL
            IY1 = 1 + (IIP-1) * MY
            IY2 = IY1 + MY - 2
            IF (NPOL.EQ.1) IY2 = NY
            CALL PLCOL3 (COLV, ICOL)
            COLV = COLV + DCOLV
            DO 30 IY = IY1,IY2
               DO 20 IX = IX1,IX2
                  TVPLAN(IX,IY,1) = ICOL(1)
                  TVPLAN(IX,IY,2) = ICOL(2)
                  TVPLAN(IX,IY,3) = ICOL(3)
 20               CONTINUE
 30            CONTINUE
 80         CONTINUE
 100     CONTINUE
C                                       draw the array
      IF (DOTV) THEN
         CALL DRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
      ELSE
         CALL GRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
         END IF
      IF (IRET.NE.0) GO TO 975
C                                       Draw border
      CALL GLTYPE (1, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Stokes labels: Y axis
      IF (OPCODE.NE.'ALIF') THEN
         SX = (TRC(2) - BLC(2)) / NPOL
         SX = SX / 2.0
         RX = SX
         DY = -0.5
         DX = -2.5
         DO 110 IIP = 1,NPOL
            CALL GPOS (BLC(1), SX, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
            CALL GCHAR (2, 0, DX, DY, CPOL(IIP), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
            SX = SX + 2.0*RX
 110        CONTINUE
         END IF
C                                       Frequency, IF: X axis
      IF (DOTV) THEN
         CALL DRAXIS (NX, MX, IRET)
      ELSE
         CALL GRAXIS (NX, MX, IRET)
         END IF
      IF (IRET.NE.0) GO TO 970
C                                       Top labels: date and time
      DX = 0.0
      DY = CHOUT(4) - 1.5
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, TIME, DATE)
      WRITE (TEXT,1030) VER, DATE, TIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      DY = DY - 1.333
C                                       top: type
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      WRITE (TEXT,1100) PRTASK, OPCODE
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Done: finish plot
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.NE.0) GO TO 975
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1900) VER
         CALL MSGWRT (2)
         END IF
      IRET = 0
      GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 990
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IRET)
         CALL ZDESTR (DISKIN, PFILE, IRET)
         END IF
      GO TO 999
C
 990  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COLOR NOT USED WITH NUMBER POLARIZATIONS AND IFS',
     *   ' REQUESTED')
 1030 FORMAT ('Plot file version',I4,' _created ',A12,A8)
 1100 FORMAT ('3-COLOR PLOT FOR TASK ',A6,' OPCODE = ',A)
 1900 FORMAT ('Plot file version',I4,' created')
 1970 FORMAT ('CANBPL: ERROR DURING GRAPHING. WILL TRY TO FINISH',
     *   ' PARTIAL GRAPH')
      END
      SUBROUTINE CSNIFS (NX, NY, NZ, TVPLAN, IRET)
C-----------------------------------------------------------------------
C   CSNIFS plots the color scheme used by SNIFS
C   Inputs:
C      NX       I      Number X pixels in TVPLAN
C      NY       I      Number Y pixels in TVPLAN
C      NZ       I      Number Z pixels in TVPLAN
C   In/out
C      TVPLAN   I(*)   Image memory - 0 on input
C   Output
C      IRET     I      Erroro code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), IRET
C
      INCLUDE 'PLOTC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INTEGER   IX, IY, MY, ICOL(3), JJJ, IY1, IY2, IX1, IX2, ID(3),
     *   IT(3), INCHAR, SOUKOL, NKEY, NREC, NCOL, I, SUBA, KOLS(3),
     *   DATP(128,2), NROWS, GNRECI(XCLRSZ), SID(500), NID, NSOUR,
     *   ISLUN, ISOU, NCOLOR, TIMKOL, ANTKOL, REFANT, IANT, IIP, JX, MX,
     *   IQUAL(30)
      REAL      COLV, DCOLV, DX, DY, SX, RX, GNREC(XCLRSZ), TIME, TBEG
      CHARACTER CTIME*8, DATE*12, TEXT*80, COLTAB(3)*24
      DOUBLE PRECISION GNRECD(XCLRSZ/2)
      EQUIVALENCE (GNRECD, GNREC, GNRECI)
      DATA COLTAB /'SOURCE ID', 'TIME', 'ANTENNA NO.'/
      DATA ISLUN /25/
C-----------------------------------------------------------------------
C                                       all antenna - just uses max ant
      IF (OPCODE.EQ.'ALAN') THEN
         SUBA = 1
         CALL GETANT (DISKIN, CNOIN, SUBA, CATBLK, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING ANTENNA TABLE'
            GO TO 980
            END IF
         NCOLOR = NSTNS
         CHIF = 'ANTENNA'
C                                       count = color, must read table
      ELSE
         OPCODE = 'ALTI'
         CHIF = 'SAMPLE'
         CALL FNDEXT (INEXT, CATBLK, JJJ)
         IF (JJJ.LE.0) THEN
            MSGTXT = 'NO TABLES OF TYPE ' // INEXT // ' FOUND'
            IRET = 9
            GO TO 980
            END IF
         IF (INVERS.LE.0) INVERS = JJJ
         IF (INVERS.GT.JJJ) INVERS = JJJ
C                                       Look up sources
         NID = 500
         NSOUR = 30
         MSGSUP = 32000
         CALL FILL (30, -1, IQUAL)
         CALL SOURNU (SOURCS, IQUAL, NSOUR, DISKIN, CNOIN, NID, BUFF1,
     *      SID, IRET)
         MSGSUP = 0
         IF (IRET.LT.0) THEN
            MSGTXT = 'SPECIFIED SOURCE(S) NOT FOUND - CONTINUING'
            CALL MSGWRT (6)
            END IF
         IF (IRET.NE.0) NID = 0
C                                       Open table
         ICLUN = 28
         NKEY = 0
         NREC = 0
         NCOL = 0
         ICLRNO = 1
         CALL TABINI ('READ', INEXT, DISKIN, CNOIN, INVERS, CATBLK,
     *      ICLUN, NKEY, NREC, NCOL, DATP, CLBUFF, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1001) IRET, INEXT, INVERS
            GO TO 980
            END IF
C                                       Get number of records
         NCLINR = CLBUFF(5)
         NROWS = NCLINR
         CALL FNDCOL (3, COLTAB, 24, .TRUE., CLBUFF, KOLS, IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.10)) THEN
            MSGTXT = 'REQUIRED COLUMNS NOT FOUND'
            GO TO 980
            END IF
         SOUKOL = DATP(KOLS(1),1)
         TIMKOL = DATP(KOLS(2),1)
         ANTKOL = DATP(KOLS(3),1)
         IF (SOLINT.LE.0.0) SOLINT = 600
         SOLINT = SOLINT / (24.0 * 3600.0)
         REFANT = XREF + 0.1
         REFANT = MAX (1, REFANT)
         TBEG = -100.0
C                                       get source numbers used
         NCOLOR = 0
         DO 100 ICLRNO = 1,NCLINR
            CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IRET)
            IF (IRET.LT.0) GO TO 100
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING TABLE'
               GO TO 990
               END IF
            ISOU = GNRECI(SOUKOL)
            IANT = GNRECI(ANTKOL)
            IF (IANT.NE.REFANT) GO TO 100
C                                       source selection
            IF (NID.GT.0) THEN
               DO 10 I = 1,NID
                  IF (ISOU.EQ.SID(I)) GO TO 20
 10               CONTINUE
               GO TO 100
               END IF
 20         IF (INEXT.EQ.'TY') THEN
               TIME = GNREC(TIMKOL)
            ELSE
               TIME = GNRECD(TIMKOL)
               END IF
            IF (TBEG.LT.-10.) THEN
               TBEG = TIME
               NCOLOR = 1
            ELSE IF (TIME.GT.TBEG+SOLINT) THEN
               GO TO 105
            ELSE
               NCOLOR = NCOLOR + 1
               END IF
 100        CONTINUE
 105     CALL TABIO ('CLOS', 0, ICLRNO, GNREC, CLBUFF, IRET)
         END IF
      IF (NCOLOR.LE.0) THEN
         MSGTXT = 'NO RECORDS FOUND: CHECK INEXT, SOURCES'
         IRET = 8
         GO TO 980
         END IF
C                                       so do it
      NPOL = MIN (2, NPOL)
      NCOLOR = NCOLOR * NPOL
      DCOLV = 0.97 / MAX (1, NCOLOR-1)
      IRET = 0
C                                       sizes of things
      NCOLOR = NCOLOR / NPOL
      MX = NX / NCOLOR
      MY = NY / NPOL
C                                       do it
      COLV = 0.0
      JX = 1
      DO 150 ISOU = 1,NCOLOR
         IX1 = JX
         IX2 = JX + MAX (0, MX-2)
         JX = JX + MX
         DO 145 IIP = 1,NPOL
            IY1 = 1 + (IIP-1) * MY
            IY2 = IY1 + MY - 2
            IF (NPOL.EQ.1) IY2 = NY
            CALL PLCOL3 (COLV, ICOL)
            COLV = COLV + DCOLV
            DO 140 IY = IY1,IY2
               DO 130 IX = IX1,IX2
                  TVPLAN(IX,IY,1) = ICOL(1)
                  TVPLAN(IX,IY,2) = ICOL(2)
                  TVPLAN(IX,IY,3) = ICOL(3)
 130              CONTINUE
 140           CONTINUE
 145        CONTINUE
 150     CONTINUE
C                                       draw the array
      IF (DOTV) THEN
         CALL DRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
      ELSE
         CALL GRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
         END IF
      IF (IRET.NE.0) GO TO 975
C                                       Draw border
      CALL GLTYPE (1, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Stokes labels: Y axis
      SX = (TRC(2) - BLC(2)) / NPOL
      SX = SX / 2.0
      RX = SX
      DY = -0.5
      DX = -2.5
      DO 160 IIP = 1,NPOL
         CALL GPOS (BLC(1), SX, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GCHAR (2, 0, DX, DY, CPOL(IIP), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         SX = SX + 2.0*RX
 160     CONTINUE
C                                       Frequency, IF: X axis
      BCHAN = 1
      ECHAN = NCOLOR
      BIF = 0
      EIF = -1
      IF (DOTV) THEN
         CALL DRAXIS (NX, MX, IRET)
      ELSE
         CALL GRAXIS (NX, MX, IRET)
         END IF
      IF (IRET.NE.0) GO TO 970
C                                       Top labels: date and time
      DX = 0.0
      DY = CHOUT(4) - 1.5
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, CTIME, DATE)
      WRITE (TEXT,1030) VER, DATE, CTIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      DY = DY - 1.333
C                                       top: type
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      TEXT = '3-COLOR PLOT FOR TASK ' // PRTASK // ' OPCODE = ' //
     *   OPCODE
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Done: finish plot
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.NE.0) GO TO 975
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1900) VER
         CALL MSGWRT (2)
         END IF
      IRET = 0
      GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 990
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IRET)
         CALL ZDESTR (DISKIN, PFILE, IRET)
         END IF
      GO TO 999
C
 980  CALL MSGWRT (8)
      GO TO 970
C
 990  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CSNIFS ERROR',I4,' ON ',A)
 1001 FORMAT ('CSNIFS ERROR ',I3,' OPENING ',A,' TABLE NO. ',I3)
 1030 FORMAT ('Plot file version',I4,' _created ',A12,A8)
 1900 FORMAT ('Plot file version',I4,' created')
 1970 FORMAT ('CSNPLT: ERROR DURING GRAPHING. WILL TRY TO FINISH',
     *   ' PARTIAL GRAPH')
      END
      SUBROUTINE CBPEDT (NX, NY, NZ, TVPLAN, IRET)
C-----------------------------------------------------------------------
C   CBPEDT plots the color scheme used by BPEDT and PCEDT
C   Inputs:
C      NX       I      Number X pixels in TVPLAN
C      NY       I      Number Y pixels in TVPLAN
C      NZ       I      Number Z pixels in TVPLAN
C   In/out
C      TVPLAN   I(*)   Image memory - 0 on input
C   Output
C      IRET     I      Erroro code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), IRET
C
      INCLUDE 'PLOTC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DTVC.INC'
      INTEGER   IX, IY, MY, ICOL(3), JJJ, IY1, IY2, IX1, IX2, ID(3), I,
     *   IT(3), INCHAR, NKEY, NREC, NCOL, KOLS(2), DATP(128,2), NROWS,
     *   GNRECI(XBPRSZ),ISLUN, ISOU, NCOLOR, TIMKOL, ANTKOL, REFANT,
     *   IANT, IIP, JX, MX, JTT(4), MAXCOL
      REAL      COLV, DCOLV, DX, DY, SX, RX, GNREC(XBPRSZ), TIME,
     *   TIMES(500), TEMP
      CHARACTER CTIME*8, DATE*12, TEXT*80, COLBP(2)*24, COLPC(2)*24,
     *   COLTAB(2)*24, TSIGN*1
      DOUBLE PRECISION GNRECD(XBPRSZ/2)
      EQUIVALENCE (GNRECD, GNREC, GNRECI)
      DATA COLBP /'TIME', 'ANTENNA'/
      DATA COLPC /'TIME', 'ANTENNA_NO'/
      DATA ISLUN /25/
C-----------------------------------------------------------------------
      IF (PRTASK.EQ.'PCEDT') THEN
         INEXT = 'PC'
         COLTAB(1) = COLPC(1)
         COLTAB(2) = COLPC(2)
      ELSE
         INEXT = 'BP'
         COLTAB(1) = COLBP(1)
         COLTAB(2) = COLBP(2)
         END IF
      IF (DOTV) THEN
         MAXCOL = (WINDtv(4) - WINDTV(2)) / CSIZTV(2) - 2
      ELSE
         MAXCOL = 90
         END IF
      CALL FNDEXT (INEXT, CATBLK, JJJ)
      IF (JJJ.LE.0) THEN
         MSGTXT = 'NO TABLES OF TYPE ' // INEXT // ' FOUND'
         IRET = 9
         GO TO 980
         END IF
      IF (INVERS.LE.0) INVERS = JJJ
      IF (INVERS.GT.JJJ) INVERS = JJJ
C                                       Open table
      ICLUN = 28
      NKEY = 0
      NREC = 0
      NCOL = 0
      ICLRNO = 1
      CALL TABINI ('READ', INEXT, DISKIN, CNOIN, INVERS, CATBLK,
     *   ICLUN, NKEY, NREC, NCOL, DATP, CLBUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1001) IRET, INEXT, INVERS
         GO TO 980
         END IF
C                                       Get number of records
      NCLINR = CLBUFF(5)
      NROWS = NCLINR
      CALL FNDCOL (2, COLTAB, 24, .TRUE., CLBUFF, KOLS, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.10)) THEN
         MSGTXT = 'REQUIRED COLUMNS NOT FOUND'
         GO TO 980
         END IF
      TIMKOL = DATP(KOLS(1),1)
      ANTKOL = DATP(KOLS(2),1)
      REFANT = XREF + 0.1
      REFANT = MAX (1, REFANT)
C                                       get times used
      NCOLOR = 0
      DO 100 ICLRNO = 1,NCLINR
         CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IRET)
         IF (IRET.LT.0) GO TO 100
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING TABLE'
            GO TO 990
            END IF
         IANT = GNRECI(ANTKOL)
         IF (IANT.NE.REFANT) GO TO 100
         TIME = GNRECD(TIMKOL)
         IF (TIME.GT.TFIN) THEN
            GO TO 105
         ELSE IF (TIME.GE.TSTR) THEN
            NCOLOR = NCOLOR + 1
            TIMES(NCOLOR) = TIME
            IF (NCOLOR.EQ.MAXCOL) THEN
               MSGTXT = 'MAXCOL EXCEEDED: ONLY SOME TIMES SHOWN'
               CALL MSGWRT (6)
               GO TO 105
               END IF
            END IF
 100     CONTINUE
 105  CALL TABIO ('CLOS', 0, ICLRNO, GNREC, CLBUFF, IRET)
      IF (NCOLOR.LE.0) THEN
         MSGTXT = 'NO RECORDS FOUND: CHECK INVERS, REFANT'
         IRET = 8
         GO TO 980
         END IF
C                                       so do it
      DCOLV = 0.97 / MAX (1, NCOLOR-1)
      IRET = 0
C                                       sizes of things
      MX = NX
      MY = NY / NCOLOR
C                                       do it
      COLV = 0.0
      JX = 1
      DO 150 ISOU = 1,NCOLOR
         IX1 = JX
         IX2 = JX + MX - 1
         IY1 = 1 + (ISOU-1) * MY
         IY2 = IY1 + MY - 2
         CALL PLCOL3 (COLV, ICOL)
         COLV = COLV + DCOLV
         DO 140 IY = IY1,IY2
            DO 130 IX = IX1,IX2
               TVPLAN(IX,IY,1) = ICOL(1)
               TVPLAN(IX,IY,2) = ICOL(2)
               TVPLAN(IX,IY,3) = ICOL(3)
 130           CONTINUE
 140        CONTINUE
 150     CONTINUE
C                                       draw the array
      IF (DOTV) THEN
         CALL DRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
      ELSE
         CALL GRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IRET)
         END IF
      IF (IRET.NE.0) GO TO 975
C                                       Draw border
      CALL GLTYPE (1, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Stokes labels: Y axis
      TEMP = MY * NCOLOR
      TEMP = TEMP / NY
      SX = TEMP * (TRC(2) - BLC(2)) / NCOLOR
      SX = SX / 2.0
      RX = SX
      DY = -0.5
      CALL T2DHMS (TIMES(NCOLOR), TSIGN, JTT)
      IF (JTT(1).GT.99) THEN
         I = 1
      ELSE IF (JTT(1).GT.9) THEN
         I = 2
      ELSE IF (JTT(1).GT.0) THEN
         I = 3
      ELSE
         I = 5
         END IF
      DX = -13.5 + I
      DO 160 IIP = 1,NCOLOR
         CALL GPOS (BLC(1), SX, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL T2DHMS (TIMES(IIP), TSIGN, JTT)
         WRITE (TEXT,1160) JTT
         CALL GCHAR (13-I, 0, DX, DY, TEXT(I:12), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         SX = SX + 2.0*RX
 160     CONTINUE
C                                       Top labels: date and time
      DX = 0.0
      DY = CHOUT(4) - 1.5
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, CTIME, DATE)
      WRITE (TEXT,1030) VER, DATE, CTIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      DY = DY - 1.333
C                                       top: type
      CALL GPOS (BLC(1), TRC(2), BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      TEXT = '3-COLOR PLOT FOR TASK ' // PRTASK
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Done: finish plot
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.NE.0) GO TO 975
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1900) VER
         CALL MSGWRT (2)
         END IF
      IRET = 0
      GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 990
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IRET)
         CALL ZDESTR (DISKIN, PFILE, IRET)
         END IF
      GO TO 999
C
 980  CALL MSGWRT (8)
      GO TO 970
C
 990  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CBPEDT ERROR',I4,' ON ',A)
 1001 FORMAT ('CBPEDT ERROR ',I3,' OPENING ',A,' TABLE NO. ',I3)
 1030 FORMAT ('Plot file version',I4,' _created ',A12,A8)
 1160 FORMAT (I3,'/',2(I2.2,':'),I2.2)
 1900 FORMAT ('Plot file version',I4,' created')
 1970 FORMAT ('CBPEDT: ERROR DURING GRAPHING. WILL TRY TO FINISH',
     *   ' PARTIAL GRAPH')
      END
