LOCAL INCLUDE 'RSPEC.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, DISKIN, CNOIN, NUMHIS, JBUFSZ, LUNI, INDI,
     *   WIN(4), PVER, GLUN, GFIND, PLBUFF(256), ZINC, LTYPE, NZI,
     *   NPARM, GRCHN, TVCHN, TVCORN(4), LUNP, FINDP, BUFFER(256),
     *   IPCNT, PAGE, NACROS, NGOOD, OMITW(4), SEQOU, DISKOU, CNOUT,
     *   CATIN(256), LUNO, INDO, ILABEL
      LOGICAL   DOTV, FLUXFL, INVERT, OBLANK, DOWEIT, ISFQID
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMEO(3), XCLAOU(2), XLPNAM(12),
     *   XOPTYP, XOPCOD
      CHARACTER NAMEIN*12, CLAIN*6, HISCRD(10)*64, LPNAME*48, TITL1*132,
     *   TITL2*132, LINE*132, SCRTCH*132, OPTYPE*4, NAMEOU*12, CLAOU*6,
     *   OPCODE*4
      REAL      USERID, XSEQIN, XDISKI, BLC(7), TRC(7), DOOUT, XOUTS,
     *   XOUTD, PIXR(2), XZINC, XSMOTH(3), XLTYPE, DOCENT, XDOTV, XGRCH,
     *   DOSLIC, XYRATO, XSUM, BUFF1(MABFSS), RANGE(2), DBUFF(MAXIMG),
     *   DOCRT, CBAREA, DSUM, FSUM, BUFF2(MABFSS), OMAX, OMIN, WEIMAX
      DOUBLE PRECISION FRQS(MAXIMG)
      COMMON /INPARM/ USERID, XNAMEI, XCLAIN, XSEQIN, XDISKI, BLC, TRC,
     *   XOPTYP, XOPCOD, DOOUT, XNAMEO, XCLAOU, XOUTS, XOUTD, PIXR,
     *   XZINC, XSMOTH, XLTYPE, DOCENT, XDOTV, XGRCH, DOCRT, XLPNAM,
     *   DOSLIC, XYRATO
      COMMON /CHPARM/ NAMEIN, CLAIN, OPTYPE, OPCODE, NAMEOU, CLAOU,
     *   HISCRD, LPNAME, TITL1, TITL2, LINE, SCRTCH
      COMMON /PARMS/ FRQS, CATIN, RANGE, SEQIN, DISKIN, CNOIN, LUNI,
     *   INDI, ZINC, LTYPE, JBUFSZ, NUMHIS, WIN, PVER, GLUN, GFIND, NZI,
     *   NPARM, GRCHN, TVCHN, TVCORN, DOTV, LUNP, FINDP, IPCNT, PAGE,
     *   NACROS, CBAREA, FLUXFL, INVERT, FSUM, DSUM, XSUM, NGOOD, OMITW,
     *   SEQOU, DISKOU, CNOUT, LUNO, INDO, OMIN, OMAX, OBLANK, DOWEIT,
     *   WEIMAX, ISFQID, ILABEL
      COMMON /BUFRS/ PLBUFF, BUFF1, BUFF2, DBUFF, BUFFER
LOCAL END
      PROGRAM RSPEC
C-----------------------------------------------------------------------
C! Task to plot the spectrum of rms, write S/N image
C# Map-util Utility Spectral Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 2010-2012, 2014-2016
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
C   RSPEC allows a user to specify a region or pixel and then will
C   generate the spectrum of the rms of that region.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      PIXRANGE       PIXR          Range of intensities to plot
C      ZINC           ZINC          Increment on freq axis
C      LTYPE          LTYPE         Type of labelling
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, IERR
      INTEGER I
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'RSPEC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA PRGM /'RSPEC '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL ISPCIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Obtain spectrum
      CALL GETSPC (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       HI, etc for output image
      IF (DOOUT.GT.0.0) CALL RSPECH
      CALL COPY (256, CATIN, CATBLK)
C                                       recalculate RMS to weight and
C                                       calculate maximum weight;
C                                       normalize by this maximum
      IF (DOWEIT) THEN
         WEIMAX = 1.E-10
         DO 25 I = 1,NZI,ZINC
            IF (DBUFF(I) .NE. 0) DBUFF(I) = 1/(DBUFF(I)**2)
            WEIMAX = MAX(WEIMAX, DBUFF(I))
   25       CONTINUE
C
         RANGE(1) = 1.0E20
         RANGE(2) = -1.0E20
         DO 30 I = 1,NZI,ZINC
            IF (DBUFF(I).NE.FBLANK) THEN
               DBUFF(I) = DBUFF(I) / WEIMAX * 100
C                                       min,max for TV
               RANGE(1) = MIN(RANGE(1), DBUFF(I))
               RANGE(2) = MAX(RANGE(2), DBUFF(I))
               END IF
   30       CONTINUE
         END IF
C                                       Obtain spectrum
      IF (DOSLIC.GT.0.0) THEN
         CALL SLISPC (IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       Plot it
      CALL PLTSCH (IRET)
      CALL PLTSPC (IRET)
C                                       Close printer
      IF (DOCRT.NE.0.0) THEN
         CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
         IF (IRET.EQ.0) IRET = IERR
         END IF
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE ISPCIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   ISPCIN gets input parameters for RSPEC, creates 'PL' file and
C   sets up scaling etc.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                      4 => user routine detected error.
C                      5 => catalog troubles
C                      8 => can't start
C   Commons:
C      /INPARM/ all input adverbs in order given by INPUTS file
C      /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, MTYPE*2, UNITS*8, CBLANK*8, CTEMP*8
      INTEGER   IROUND, IUSER, DEPTH(5), NFQ, ORDER, I, J
      REAL      TEMP
      LOGICAL   FULL
      DOUBLE PRECISION DTEMP
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'RSPEC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA DEPTH /5*1/
      DATA CBLANK /' '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 57
      CALL GTPARM (PRGN, NPARM, RQUICK, USERID, BUFF1, IRET)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (DOCRT.NE.0.0) DOCRT = MIN (-1.0, DOCRT)
         END IF
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF ((DOCRT.NE.0.0) .AND. (RQUICK)) RQUICK = LPNAME.NE.' '
      IF (IRET.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IRET.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IRET
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IUSER)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      IF ((OPTYPE.NE.'LOW') .AND. (OPTYPE.NE.'MEDI')) OPTYPE = ' '
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
C
      DOWEIT = OPCODE .EQ. 'WEIT'
      CALL CHR2H (4, OPTYPE, 1, XOPTYP)
      IF (XYRATO.LE.0.0) XYRATO = 1.3
C                                       Crunch input parameters.
      USERID = NLUSER
      IUSER = NLUSER
      SEQIN = IROUND (XSEQIN)
      DISKIN = IROUND (XDISKI)
      INVERT = XZINC.LT.0.0
      ZINC = IROUND (XZINC)
      ZINC = ABS (ZINC)
      IF (ZINC.LE.0) ZINC = 1
      XZINC = ZINC
      IF (INVERT) XZINC = -XZINC
      LTYPE = IROUND (XLTYPE)
      ILABEL = MOD (ABS(LTYPE), 100)
      IF ((ILABEL.EQ.0) .OR. (ILABEL.GT.10)) THEN
         ILABEL = 3
         IF (LTYPE.GE.0) THEN
            LTYPE = (LTYPE/100) * 100 + ILABEL
         ELSE
            LTYPE = (LTYPE/100) * 100 - ILABEL
            END IF
         END IF
      XLTYPE = LTYPE
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
C                                       Open map file and get
C                                       CATBLK
      LUNI = 16
      MTYPE = 'MA'
      STAT = 'HDWR'
      IF ((DOTV) .AND. (DOSLIC.LE.0.0)) STAT = 'READ'
      CALL MAPOPN (STAT, DISKIN, NAMEIN, CLAIN, SEQIN, MTYPE, IUSER,
     *   LUNI, INDI, CNOIN, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL COPY (256, CATBLK, CATIN)
      NCFILE = 1
      FVOL(1) = DISKIN
      FCNO(1) = CNOIN
      FRW(1) = 1
      IF (STAT.EQ.'READ') FRW(1) = 0
C                                       Check number of pts, planes
      FULL = .FALSE.
      IF (BLC(1)+2.0.GE.TRC(1)) THEN
         TRC(1) = 0
         BLC(1) = 0
         FULL = .TRUE.
         END IF
      IF (BLC(2)+2.0.GE.TRC(2)) THEN
         TRC(2) = 0
         BLC(2) = 0
         FULL = .TRUE.
         END IF
      IF (BLC(3).EQ.TRC(3)) THEN
         TRC(3) = 0
         BLC(3) = 0
         END IF
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IRET)
      OMITW(1) = BLC(1) + 0.01
      OMITW(3) = TRC(1) + 0.01
      OMITW(2) = BLC(2) + 0.01
      OMITW(4) = TRC(2) + 0.01
      IF (OMITW(3)-OMITW(1)+1.GT.CATBLK(KINAX)-3) FULL = .TRUE.
      IF (OMITW(4)-OMITW(2)+1.GT.CATBLK(KINAX+1)-3) FULL = .TRUE.
      IF (FULL) THEN
         OMITW(3) = -1
         OMITW(4) = -1
         OMITW(1) = CATBLK(KINAX) + 1
         OMITW(2) = CATBLK(KINAX+1) + 1
         END IF
      BLC(1) = 1.0
      BLC(2) = 1.0
      TRC(1) = CATBLK(KINAX)
      TRC(2) = CATBLK(KINAX+1)
C                                       check coordinates
      LOCNUM = 1
      CALL SETLOC (DEPTH, .TRUE.)
      CALL H2CHR (8, 1, CATH(KHBUN), UNITS)
      CALL CHLTOU (8, UNITS)
      TEMP = CATR(KRCIC) * CATR(KRCIC+1)
      CBAREA = 1.1331 * CATR(KRBMJ) * CATR(KRBMN)
      FLUXFL = (AXTYP(LOCNUM).EQ.1) .AND. (UNITS.EQ.'JY/BEAM') .AND.
     *   (TEMP.NE.0.0) .AND. (CBAREA.GT.0.0)
      IF (FLUXFL) THEN
         CBAREA = CBAREA / ABS (TEMP)
      ELSE
         CBAREA = 1.0
         END IF
C                                       Set the I/O windows used by
C                                       MINIT/MDISK
      IF (IRET.EQ.0) THEN
         WIN(1) = IROUND(BLC(1))
         WIN(2) = IROUND(BLC(2))
         WIN(3) = IROUND(TRC(1))
         WIN(4) = IROUND(TRC(2))
         END IF
C                                       Open printer
      IF (DOCRT.NE.0.0) THEN
         PAGE  = 0
         IPCNT = 980
         TITL1 = ' '
         TITL2 = ' '
         LINE  = ' '
         IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
         CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET
            IRET = 1
            GO TO 990
            END IF
         END IF
C                                       FQID axis?
      CALL H2CHR (8, 1, CATH(KHCTP+4), CTEMP)
      ISFQID = CTEMP.EQ.'FQID'
      IF (ISFQID) THEN
         CALL FRQGET (DISKIN, CNOIN, NFQ, ORDER, FRQS, IRET)
         IF ((IRET.NE.0) .OR. (ABS(ORDER).NE.1)) THEN
            MSGTXT = 'FQID AXIS NOT CHANGED TO FREQUENCIES'
            CALL MSGWRT (7)
            ISFQID = .FALSE.
         ELSE
            NZI = TRC(3) - BLC(3) + 1.1
            J = BLC(3) - 0.5
            DO 10 I = 1,NZI
               FRQS(I) = FRQS(I+J)
 10            CONTINUE
            IF (INVERT) THEN
               DO 20 I = 1,NZI/2
                  DTEMP = FRQS(I)
                  FRQS(I) = FRQS(NZI+1-I)
                  FRQS(NZI+1-I) = DTEMP
 20               CONTINUE
               END IF
            END IF
         END IF
C                                       create output image
      IF (DOOUT.GT.0.0) THEN
         OBLANK = .FALSE.
         OMIN = 1.E10
         OMAX = -1.E10
         LUNO = 17
         CALL H2CHR (12, 1, XNAMEO, NAMEOU)
         CALL H2CHR (6, 1, XCLAOU, CLAOU)
         SEQOU = XOUTS + 0.1
         DISKOU = XOUTD + 0.1
C                                       Build new file cat name.
         CALL MAKOUT (NAMEIN, CLAIN, SEQIN, CBLANK, NAMEOU, CLAOU,
     *      SEQOU)
C                                       Set header values needed
C                                       by MCREAT.
         CALL SUBHDR (BLC, TRC, 1.0, 1.0)
         CALL CHR2H (12, NAMEOU, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLAOU, KHIMCO, CATH(KHIMC))
         CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
         CATBLK(KIIMS) = SEQOU
C                                       Create new cataloged file.
         CALL MCREAT (DISKOU, CNOUT, DBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         SEQOU = CATBLK(KIIMS)
C                                       Copy any header keywords
         CALL KEYCOP (DISKIN, CNOIN, DISKOU, CNOUT, IRET)
         CALL MAPOPN ('INIT', DISKOU, NAMEOU, CLAOU, SEQOU, MTYPE,
     *      IUSER, LUNO, INDO, CNOUT, CATBLK, BUFF2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1110) IRET, NAMEOU, CLAOU, SEQOU, DISKOU,
     *         NLUSER
            GO TO 990
            END IF
         NCFILE = 2
         FVOL(2) = DISKOU
         FCNO(2) = CNOUT
         FRW(1) = 2
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ISPCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1080 FORMAT ('ISPCIN: ERROR ',I3,' OPENING OUTPUT ''PRINT'' DEVICE')
 1100 FORMAT ('ISPCIN: ERROR ',I4,' CREATING OUTPUT IMAGE CUBE')
 1110 FORMAT ('ERROR',I3,' OPENING ',A12,'.',A6,'.',I3,' DISK',
     *   I3,' USID',I5,' OUTPUT')
      END
      SUBROUTINE GETSPC (IRET)
C-----------------------------------------------------------------------
C   GETSPC fills up the plotting buffer with the spectrum to be plotted
C   this is then passed to the plotting routine via common.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   NYI, NXI, BOI, LIMO, LIM2, I3, NX, NY, IPOS(7), BOTEMP,
     *   NWORDS, K, OPOS(7), NXO, NYO, WINO(4)
      REAL      MAPBUF(2), WRKBUF(2)
      LONGINT   PMAPB, PWORK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'RSPEC.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      K = 2
      IF (OPTYPE.EQ.'LOW') K = 1
C                                       Setup for I/O
      NXI = CATIN(KINAX)
      NYI = CATIN(KINAX+1)
      NZI = TRC(3) - BLC(3) + 1.01
      CALL RFILL (NZI, 0.0, DBUFF)
      DSUM = 0.0
      FSUM = 0.0
      XSUM = 0.0
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
C                                       define work space
      NX = TRC(1) - BLC(1) + 1.01
      NY = TRC(2) - BLC(2) + 1.01
      NWORDS = (NX * NY - 1) / 1024 + 2
      CALL ZMEMRY ('GET ', 'GETSPC', NWORDS, MAPBUF, PMAPB, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET REQUIRED DYNAMIC MEMORY'
         GO TO 990
         END IF
      CALL ZMEMRY ('GET ', 'GETSPC', NWORDS, WRKBUF, PWORK, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET REQUIRED DYNAMIC MEMORY'
         GO TO 990
         END IF
C                                       Setup for looping
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIMO = CATIN(KINAX) - 1
      RANGE(1) = 1.0E20
      RANGE(2) = -1.0E20
C                                       Loop
      IPOS(7) = BLC(7) + 0.01
      IPOS(6) = BLC(6) + 0.01
      IPOS(5) = BLC(5) + 0.01
      IPOS(4) = BLC(4) + 0.01
      CALL FILL (7, 1, OPOS)
      DO 300 I3 = 1,NZI
         IF (INVERT) THEN
            IPOS(3) = BLC(3) + NZI - I3 + 0.1
            OPOS(3) = NZI - I3 + 1
         ELSE
            IPOS(3) = BLC(3) + I3 - 0.9
            OPOS(3) = I3
            END IF
C                                       Init. files, first input.
         CALL COMOFF (CATIN(KIDIM), CATIN(KINAX), IPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET
            GO TO 990
            END IF
         BOI = BOTEMP + 1
         CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WIN, BUFF1, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'READ', IRET
            GO TO 990
            END IF
C                                       find rms
         IF (OPTYPE.EQ.'MEDI') THEN
            CALL RSRMSM (NX, NY, OMITW, LUNI, INDI, BUFF1,
     *         MAPBUF(1+PMAPB), WRKBUF(1+PWORK), DBUFF(I3), IRET)
         ELSE
            CALL RSRMS (K, NX, NY, OMITW, LUNI, INDI, BUFF1,
     *         MAPBUF(1+PMAPB), DBUFF(I3), IRET)
            END IF
C                                       check for blanked pixels
         IF (IRET.GT.0) THEN
            MSGTXT = 'RSRMS RETURNS ERROR CODE'
            GO TO 990
         ELSE IF (IRET.LT.0) THEN
            DBUFF(I3) = FBLANK
            IRET = 0
         ELSE
            FSUM = FSUM + DBUFF(I3)
            XSUM = XSUM + 1.0
            DSUM = DSUM + DBUFF(I3) * DBUFF(I3)
            RANGE(1) = MIN (DBUFF(I3), RANGE(1))
            RANGE(2) = MAX (DBUFF(I3), RANGE(2))
            END IF
C                                       Init. files, first input.
         IF (DOOUT.GT.0.0) THEN
            CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WIN, BUFF1,
     *         JBUFSZ, BOI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'READ', IRET
               GO TO 990
               END IF
            CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), OPOS(3), BOTEMP,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET
               GO TO 990
               END IF
            BOI = BOTEMP + 1
            CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2,
     *         JBUFSZ, BOI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'WRIT', IRET
               GO TO 990
               END IF
            CALL RSPECO (NX, NY, LUNI, INDI, LUNO, INDO, BUFF1, BUFF2,
     *         MAPBUF(1+PMAPB), DBUFF(I3), OMIN, OMAX, OBLANK, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 300     CONTINUE
C                                       Close input map.
      CALL ZCLOSE (LUNI, INDI, I3)
      CALL ZMEMRY ('FREE', 'GETSPC', NWORDS, MAPBUF, PMAPB, I3)
      CALL ZMEMRY ('FREE', 'GETSPC', NWORDS, WRKBUF, PWORK, I3)
      IF (DOOUT.GT.0.0) THEN
         CATR(KRDMX) = OMAX
         CATR(KRDMN) = OMIN
         CATR(KRBLK) = 0.0
         IF (OBLANK) CATR(KRBLK) = FBLANK
         CALL CHR2H (8, 'SIG/NOIS', 1, CATH(KHBUN))
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETSPC: COMOFF ERROR',I3)
 1100 FORMAT ('GETSPC: INIT-FOR-',A4,' ERROR',I3)
      END
      SUBROUTINE RSRMS (K, NX, NY, OMITW, LUN, IND, BUFF, IMAGE, RMS,
     *   IRET)
C-----------------------------------------------------------------------
C   RSRMS deterimes the RMS in an image plane
C   Inputs:
C      K       I      1 => tight fit, 2 => more slack
C      NX      I      Number X points
C      NY      I      Number Y points
C      OMITW   I(4)   Omit this rectangle
C      LUN     I      Open LUN - MINIT already called
C      IND     I      FTAB pointer
C   Output:
C      BUFF    R(*)   I/O buffer
C      IMAGE   R(NX,NY)   image work space
C      RMS     R          RMS
C      IRET    I          Error code: 0 good, -1 failed, > 0 I/O error
C-----------------------------------------------------------------------
      INTEGER   K, NX, NY, OMITW(4), LUN, IND, IRET
      REAL      BUFF(*), IMAGE(NX,NY), RMS
C
      INTEGER   IX, IY, L, NS, IBIND, MX
      DOUBLE PRECISION SV, SS, V, VMIN, VMAX, VM(10,2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA VM / 6.D0, 5.D0, 4.D0, 3.D0, 2.8D0, 2.6D0, 2.4D0, 2.D0,
     *   1.5D0, 1.5D0, 6.D0, 5.D0, 4.5D0, 4.D0, 3.6D0, 3.2D0, 3.D0,
     *   2.8D0, 2.5D0, 2.5D0/
C-----------------------------------------------------------------------
      IF (K.NE.1) K = 2
      MX = OMITW(3) - OMITW(1) + 1
C                                       read in the image
      DO 10 IY = 1,NY
         CALL MDISK ('READ', LUN, IND, BUFF, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, IY
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CALL RCOPY (NX, BUFF(IBIND), IMAGE(1,IY))
         IF ((IY.GE.OMITW(2)) .AND. (IY.LE.OMITW(4)))
     *      CALL RFILL (MX, FBLANK, IMAGE(OMITW(1),IY))
 10      CONTINUE
      VMIN = -1.D10
      VMAX = 1.D10
      DO 50 L = 1,8
         SV = 0.0D0
         SS = 0.0D0
         NS = 0
         DO 30 IY = 1,NY
            DO 20 IX = 1,NX
               IF (IMAGE(IX,IY).NE.FBLANK) THEN
                  V = IMAGE(IX,IY)
                  IF ((V.GE.VMIN) .AND. (V.LE.VMAX)) THEN
                     SV = SV + V
                     SS = SS + V * V
                     NS = NS + 1
                     END IF
                  END IF
 20            CONTINUE
 30         CONTINUE
         IF (NS.LE.1) THEN
            IRET = -1
            GO TO 999
         ELSE
            SV = SV / NS
            SS = SS / NS - SV * SV
            SS = SQRT (MAX (0.0D0, SS))
            VMIN = SV - SS * VM(L,K)
            VMAX = SV + SS * VM(L,K)
            END IF
 50      CONTINUE
      RMS = SS
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MDISK   ERROR',I4,' READING ROW ',I6)
      END
      SUBROUTINE RSRMSM (NX, NY, OMITW, LUN, IND, BUFF, IMAGE, WORK,
     *   RMS, IRET)
C-----------------------------------------------------------------------
C   RSRMSM deterimes the RMS in an image plane - Wirth median method
C   Inputs:
C      NX      I          Number X points
C      NY      I          Number Y points
C      OMITW   I(4)       Omit this rectangle
C      LUN     I          Open LUN - MINIT already called
C      IND     I          FTAB pointer
C   Output:
C      BUFF    R(*)       I/O buffer
C      IMAGE   R(NX,NY)   image work space
C      RMS     R          RMS
C      IRET    I          Error code: 0 good, -1 failed, > 0 I/O error
C-----------------------------------------------------------------------
      INTEGER   NX, NY, OMITW(4), LUN, IND, IRET
      REAL      BUFF(*), IMAGE(*), WORK(*), RMS
C
      INTEGER   IX, IY, L, IBIND, NP, MX
      REAL      VMED, MEDIAN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      NP = NX * NY
      MX = OMITW(3) - OMITW(1) + 1
C                                       read in the image
      L = 1
      DO 10 IY = 1,NY
         CALL MDISK ('READ', LUN, IND, BUFF, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, IY
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CALL RCOPY (NX, BUFF(IBIND), IMAGE(L))
         IF ((IY.GE.OMITW(2)) .AND. (IY.LE.OMITW(4)))
     *      CALL RFILL (MX, FBLANK, IMAGE(L+OMITW(1)-1))
         L = L + NX
 10      CONTINUE
      L = 0
      DO 20 IX = 1,NP
         IF (IMAGE(IX).NE.FBLANK) THEN
            L = L + 1
            WORK(L) = IMAGE(IX)
            END IF
 20      CONTINUE
C                                       all blanked?
      IF (L.LE.0) THEN
         IRET = -1
         GO TO 999
         END IF
C                                       get median
      VMED = MEDIAN (L, WORK)
C                                       convert to difference
      L = 0
      DO 30 IX = 1,NP
         IF (IMAGE(IX).NE.FBLANK) THEN
            L = L + 1
            WORK(L) = ABS (IMAGE(IX) - VMED)
            END IF
 30      CONTINUE
C                                       get this median
      VMED = MEDIAN (L, WORK)
      RMS = 1.4826 * VMED
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MDISK   ERROR',I4,' READING ROW ',I6)
      END
      SUBROUTINE RSPECO (NX, NY, LUNI, INDI, LUNO, INDO, BUFFI, BUFFO,
     *   IMAGE, RMS, OMIN, OMAX, OBLANK, IRET)
C-----------------------------------------------------------------------
C   RSPECO writes out the image
C   Inputs:
C      NX      I          Number X points
C      NY      I          Number Y points
C      LUN     I          Open LUN - MINIT already called
C      IND     I          FTAB pointer
C      IMAGE   R(NX,NY)   image work space
C      RMS     R          RMS
C   In/out
C      OMIN    R          Min output
C      OMAX    R          Max output
C      OBLANK  L          blanked?
C   Output:
C      BUFF    R(*)       I/O buffer
C      IRET    I          Error code: 0 good, -1 failed, > 0 I/O error
C-----------------------------------------------------------------------
      INTEGER   NX, NY, LUNI, INDI, LUNO, INDO, IRET
      REAL      BUFFI(*), BUFFO(*), IMAGE(*), RMS, OMIN, OMAX
      LOGICAL   OBLANK
C
      INTEGER   IX, IY, L, IBIND
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       read in the image
      L = 1
      DO 10 IY = 1,NY
         CALL MDISK ('READ', LUNI, INDI, BUFFI, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, IY
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CALL RCOPY (NX, BUFFI(IBIND), IMAGE(L))
         L = L + NX
 10      CONTINUE
C                                       write
      L = 1
      DO 30 IY = 1,NY
         CALL MDISK ('WRIT', LUNO, INDO, BUFFO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRIT', IY
            CALL MSGWRT (8)
            GO TO 999
            END IF
         IF ((RMS.NE.FBLANK) .AND. (RMS.GT.0.0)) THEN
            DO 20 IX = 1,NX
               IF (IMAGE(L+IX-1).NE.FBLANK) THEN
                  BUFFO(IBIND+IX-1) = IMAGE(L+IX-1) / RMS
                  OMIN = MIN (OMIN, BUFFO(IBIND+IX-1))
                  OMAX = MAX (OMAX, BUFFO(IBIND+IX-1))
               ELSE
                  OBLANK = .TRUE.
                  BUFFO(IBIND+IX-1) = FBLANK
                  END IF
 20            CONTINUE
         ELSE
            OBLANK = .TRUE.
            CALL RFILL (NX, FBLANK, BUFFO(IBIND))
            END IF
         L = L + NX
 30      CONTINUE
      CALL MDISK ('FINI', LUNO, INDO, BUFFO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH', IY
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MDISK ERROR',I4,1X,A,'ING ROW ',I6)
      END
      SUBROUTINE RSPECH
C-----------------------------------------------------------------------
C   Writes the HI, copies tables, closes output image
C-----------------------------------------------------------------------
      INTEGER   HLUN1, HLUN2, IERR
      CHARACTER HILINE*72, NOTTYP*2
      INCLUDE 'RSPEC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA HLUN1, HLUN2 /30,31/
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
      CALL HIINIT (3)
      CALL HISCOP (HLUN1, HLUN2, DISKIN, DISKOU, CNOIN, CNOUT, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.3) GO TO 100
      IF (IERR.EQ.3) GO TO 90
C                                       RSPEC history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, HLUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 90
      CALL HENCOO (TSKNAM, NAMEOU, CLAOU, SEQOU, DISKOU, HLUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 90
      IF (OMITW(3).GT.OMITW(1)) THEN
         WRITE (HILINE,1000) TSKNAM, OMITW
         CALL HIADD (HLUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 90
         END IF
      WRITE (HILINE,1010) TSKNAM, OPTYPE
      CALL HIADD (HLUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 90
 90   CALL HICLOS (HLUN2, .TRUE., BUFF2, IERR)
C                                       Copy CC files and others
 100  CALL ALLTAB (1, NOTTYP, HLUN1, HLUN2, DISKIN, DISKOU, CNOIN,
     *   CNOUT, CATBLK, BUFF1, BUFF2, IERR)
      CALL MAPCLS ('INIT', DISKOU, CNOUT, LUNO, INDO, CATBLK, .TRUE.,
     *   BUFF2, IERR)
      NCFILE = NCFILE - 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A,'EXCLUDE WIN=',4I6,' / omit from rms')
 1010 FORMAT (A,'OPTYPE = ''',A,''' / rms method')
      END
      SUBROUTINE PLTSCH (IRET)
C-----------------------------------------------------------------------
C   counts lines of print
C   Output:
C      IRET   I   Error code: 0 => okay - can change DOCRT to 0
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'RSPEC.INC'
      CHARACTER CTEMP*20
      REAL      X
      INTEGER   I, NCOUNT, TTY(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GE.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      NCOUNT = 0
C                                       Header line for printer
      IF (DOCRT.LE.-2.5 .AND. .NOT.DOWEIT) NCOUNT = NCOUNT + 2
C
      DO 30 I = 1,NZI,ZINC
C                                       For printer: fill LINE
         NCOUNT = NCOUNT + 1
 30      CONTINUE
C
      IF (.NOT.DOWEIT) NCOUNT = NCOUNT + 6
C                                       ask if needed
      X = DOCRT
      DOCRT = 0.0
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) THEN
            X = 0.0
            IPCNT = -1
            CALL LPCLOS (LUNP, FINDP, IPCNT, I)
            END IF
      ELSE IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 990
         WRITE (SCRTCH,1030) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 990
         SCRTCH = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRTCH, 1, CTEMP, IRET)
         IF (IRET.GT.0) GO TO 990
         IF ((CTEMP(:1).NE.'y') .AND. (CTEMP(:1).NE.'Y')) THEN
            X = 0.0
            IPCNT = -1
            CALL LPCLOS (LUNP, FINDP, IPCNT, I)
            SCRTCH = 'Good choice - save trees'
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      DOCRT = X
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE PLTSPC (IRET)
C-----------------------------------------------------------------------
C   Routine which plots the contents of the DBUFF array, performs all
C   the labelling and closes down the PL file.
C   It will also write these data to the terminal, or the line printer,
C   or to an output file depending on the value of DOCRT. (GvM, 12/92).
C   Output:
C      IRET   I   Error code: 0 => okay
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'RSPEC.INC'
      CHARACTER TEXT(3)*80, CTEMP*20, PLNAME*48, ATIME*8, ADATE*12,
     *   UNIT*8
      REAL      CHOUT(4), PBLC(2), PTRC(2), XYRATI, SCALY, X, Y, OFY,
     *   DX, DY, XBLC(7), XTRC(7), RTEMP(2), YGAP, RANGE2(2), XSEP,
     *   SBUFF(MAXIMG), SMTAB(256), SV, SS, VMIN, VMAX, VM(10), V, FQDI
      INTEGER   DEPTH(5), I, INCHAR, IPTYPE, ID(3), IT(3), NTEXT,
     *   ITEMP(2), IDROP(2), NCHAR, IROUND, IERR, ZPIX, L, NS
      HOLLERITH HTEMP(4)
      DOUBLE PRECISION DTEMP, INCR, RPIX, ZVAL, RVAL, DVAL, FQDD
      LOGICAL   F, T, PENUP, PRINT, DOSMTH
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA VM / 6.0, 5.0, 4.0, 3.0, 2.8, 2.6, 2.4, 2.0, 1.3, 1.3/
C-----------------------------------------------------------------------
      PRINT = DOCRT.NE.0.0
      FQDD = 0.0D0
      FQDI = 0.0
C                                       Set up freq smoothing
      CALL SETSM (NZI, 256, XSMOTH, DOSMTH, SMTAB)
      CALL RCOPY (NZI, DBUFF, SBUFF)
      IF (DOSMTH) CALL SMOSP (NZI, XSMOTH, SMTAB, RANGE, DBUFF, SBUFF)
C                                       Add plot file to CATBLK
      PVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKIN, CNOIN, CATBLK, BUFF1, T, 'READ',
     *      PVER, IRET)
         IF (IRET.EQ.0) THEN
            WRITE (MSGTXT,1000) PVER
            CALL MSGWRT (6)
            FRW(1) = 0
         ELSE
            WRITE (MSGTXT,1001) IRET
            GO TO 990
            END IF
         END IF
C                                       Generate the plot file name
      CALL ZPHFIL ('PL', DISKIN, CNOIN, PVER, PLNAME, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         GO TO 990
         END IF
C                                       Open the plot file
      IF (PIXR(1).GE.PIXR(2)) THEN
         PIXR(1) = RANGE(1) - 0.02 * (RANGE(2) - RANGE(1))
         PIXR(2) = RANGE(2) + 0.02 * (RANGE(2) - RANGE(1))
         END IF
C                                       start on labeling as a slice
      XBLC(1) = BLC(3)
      XBLC(2) = BLC(4)
      XBLC(3) = (BLC(1) + TRC(1)) / 2.0
      XBLC(4) = (BLC(2) + TRC(2)) / 2.0
      XBLC(5) = BLC(5)
      XBLC(6) = BLC(6)
      XBLC(7) = BLC(7)
      CALL RCOPY (6, XBLC(2), XTRC(2))
      XTRC(1) = TRC(3)
      IDROP(1) = 0
      IDROP(2) = 0
C                                       swap axes 1,2 with 3,4
      CALL COPY (2, CATBLK(KINAX), ITEMP)
      CALL COPY (2, CATBLK(KINAX+2), CATBLK(KINAX))
      CALL COPY (2, ITEMP, CATBLK(KINAX+2))
      CALL RCOPY (4, CATH(KHCTP), HTEMP)
      CALL RCOPY (4, CATH(KHCTP+4), CATH(KHCTP))
      CALL RCOPY (4, HTEMP, CATH(KHCTP+4))
      CALL H2CHR (8, 1, CATH(KHBUN), UNIT(1:8))
      IF ((FLUXFL) .AND. (OPTYPE.EQ.'FLUX')) UNIT = 'JY'
      CALL CHR2H (8, UNIT, 1, CATH(KHBUN))
      DTEMP = CATD(KDCRV)
      CATD(KDCRV) = CATD(KDCRV+2)
      CATD(KDCRV+2) = DTEMP
      DTEMP = CATD(KDCRV+1)
      CATD(KDCRV+1) = CATD(KDCRV+3)
      CATD(KDCRV+3) = DTEMP
      CALL RCOPY (2, CATR(KRCIC), RTEMP)
      CALL RCOPY (2, CATR(KRCIC+2), CATR(KRCIC))
      CALL RCOPY (2, RTEMP, CATR(KRCIC+2))
      CALL RCOPY (2, CATR(KRCRP), RTEMP)
      CALL RCOPY (2, CATR(KRCRP+2), CATR(KRCRP))
      CALL RCOPY (2, RTEMP, CATR(KRCRP+2))
      CALL RCOPY (2, CATR(KRCRT), RTEMP)
      CALL RCOPY (2, CATR(KRCRT+2), CATR(KRCRT))
      CALL RCOPY (2, RTEMP, CATR(KRCRT+2))
      IF (INVERT) THEN
         CATR(KRCIC) = -CATR(KRCIC)
         CATR(KRCRP) = TRC(3) + BLC(3) - CATR(KRCRP)
         END IF
      IF ((ISFQID) .AND. (ILABEL.EQ.3)) THEN
         CATD(KDCRV) = FRQS(1)
         CATR(KRCRP) = BLC(3)
         CATR(KRCIC) = (FRQS(NZI)-FRQS(1)) / (NZI - 1)
         CALL CHR2H (8, 'FREQ    ', 1, CATH(KHCTP))
         END IF
      RVAL  = CATD(KDCRV)
      RPIX  = CATR(KRCRP)
      INCR  = CATR(KRCIC)
C                                       init SL plot
      CATR(IRRAN) = PIXR(1)
      CATR(IRRAN+1) = PIXR(2)
      SCALY = 39999.0 / (PIXR(2) - PIXR(1))
      OFY = 40000.0 - SCALY * PIXR(2)
      RANGE2(1) = SCALY * PIXR(1) + OFY
      RANGE2(2) = SCALY * PIXR(2) + OFY
      PBLC(2) = RANGE2(1)
      PTRC(2) = RANGE2(2)
      LOCNUM = 1
      CALL RFILL (4, 0.0, CHOUT)
      YGAP = 0.0
      CALL SLBINI (IDROP, NZI, PIXR, PBLC, PTRC, XBLC, XTRC, FQDD, FQDI,
     *   DEPTH, LTYPE, YGAP, CHOUT, TEXT, NTEXT)
      PBLC(1) = PBLC(1) - 0.5
      PTRC(1) = PTRC(1) + 0.5
C                                       display window
      CHOUT(2) = CHOUT(2) - NTEXT * 1.333
      NTEXT = 1
      IF (ILABEL.GT.3) NTEXT = 2
      IF (DOSMTH) NTEXT = NTEXT + 1
      IF (ILABEL.GT.6) NTEXT = 0
      CHOUT(2) = CHOUT(2) + NTEXT * 1.333
      IDROP(1) = IROUND (BLC(1))
      IDROP(2) = IROUND (TRC(1))
      ITEMP(1) = IROUND (BLC(2))
      ITEMP(2) = IROUND (TRC(2))
      IF (NTEXT.GT.0) THEN
         IF (DOSMTH) THEN
            IF (OPTYPE.EQ.'FLUX') THEN
               WRITE (TEXT(NTEXT-1),1008) 'Sum', IDROP, ITEMP
            ELSE
               WRITE (TEXT(NTEXT-1),1008) 'Average', IDROP, ITEMP
               END IF
            WRITE (TEXT(NTEXT),1009) XSMOTH
         ELSE
            IF (OPTYPE.EQ.'FLUX') THEN
               WRITE (TEXT(NTEXT),1008) 'Sum', IDROP, ITEMP
            ELSE
               WRITE (TEXT(NTEXT),1008) 'Average', IDROP, ITEMP
               END IF
            END IF
         END IF
C                                       init plot file
      IPTYPE = 34
      CALL GINIT (DISKIN, CNOIN, PLNAME, 0, IPTYPE, NPARM, USERID,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLBUFF, GLUN, GFIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       init line drawing
      XYRATI = (PTRC(2) - PBLC(2)) / (PTRC(1) - PBLC(1)) * XYRATO
      CALL GINITL (PBLC, PTRC, XYRATI, CHOUT, DEPTH, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 985
      CALL GLTYPE (1, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 985
C                                        Draw the box
      CALL GPOS (PBLC(1), PBLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL GVEC (PTRC(1), PBLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL GVEC (PTRC(1), PTRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL GVEC (PBLC(1), PTRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL GVEC (PBLC(1), PBLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 980
C                                       More labeling: x,y coords
      IF ((ILABEL.GT.1) .AND. (ILABEL.LT.7)) THEN
         CALL GPOS (PBLC(1), PTRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 980
         DX = 0.0
         DY = 0.5
         CALL H2CHR (8, 1, CATH(KHOBJ), MSGTXT)
         INCHAR = 12
         IF (MSGTXT.EQ.' ') INCHAR = 1
         IF (NCHLAB(1,LOCNUM).GT.0) THEN
            IF (INCHAR.GT.1) MSGTXT(INCHAR-1:INCHAR-1) = '_'
            MSGTXT(INCHAR:) = SAXLAB(1,LOCNUM)(:NCHLAB(1,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
            END IF
         IF (NCHLAB(2,LOCNUM).GT.0) THEN
            IF (INCHAR.GT.1) MSGTXT(INCHAR-1:INCHAR-1) = '_'
            MSGTXT(INCHAR:) = SAXLAB(2,LOCNUM)(:NCHLAB(2,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(2,LOCNUM)
            END IF
C                                       image name
         IF (INCHAR.GT.1) MSGTXT(INCHAR-1:INCHAR-1) = '_'
         CALL H2CHR (18, 1, CATH(KHIMN), CTEMP)
         CALL NAMEST (CTEMP, CATBLK(KIIMS), MSGTXT(INCHAR:), NCHAR)
         CALL REFRMT (MSGTXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, MSGTXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 980
         TITL1 = MSGTXT
C                                       Date/time, version number
         IF (LTYPE.GT.1) THEN
            CALL GPOS (PBLC(1), PTRC(2), PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 980
            DY = DY + 1.333
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (MSGTXT,1015) PVER, ADATE, ATIME
            CALL REFRMT (MSGTXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, MSGTXT, PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 980
            END IF
C                                       Text at bottom
         IF (NTEXT.GT.0) THEN
            DX = 0.
            DY = -YGAP
            DO 20 I = 1,NTEXT
               CALL GPOS (PBLC(1), PBLC(2), PLBUFF, IRET)
               IF (IRET.NE.0) GO TO 980
               CALL CHTRIM (TEXT(I), 80, TEXT(I), INCHAR)
               CALL GCHAR (INCHAR, 0, DX, DY, TEXT(I), PLBUFF, IRET)
               IF (IRET.NE.0) GO TO 980
               DY = DY - 1.333
 20            CONTINUE
            END IF
         END IF
C                                       Axis labels and ticks
      CALL CLAB1 (PBLC, PTRC, CHOUT, LTYPE, XYRATI, F, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 980
C                                       Header line for printer
      IF (PRINT) THEN
         LINE(1:)   = 'pixel'
         LINE(10:)  = 'coord.value '
         IF (OPTYPE.EQ.'FLUX') THEN
            LINE(28:) = 'sum over area'
         ELSE
            LINE(28:) = 'avg over area'
            END IF
         IF (DOSMTH) LINE(45:) = '  freq smoothed'
         TITL2 = LINE
C         IF (DOCRT.LE.-2.5) THEN
         IF (DOCRT.LE.-2.5 .AND. .NOT.DOWEIT) THEN
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         TITL1, IPCNT, PAGE, SCRTCH, IRET)
            PRINT = IRET.EQ.0
            IF (PRINT) THEN
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            TITL2, IPCNT, PAGE, SCRTCH, IRET)
               PRINT = IRET.EQ.0
               END IF
            END IF
         END IF
C                                       Plot intensities
      CALL GLTYPE (2, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 985
      PENUP = .TRUE.
      XSEP = 0.0
      IF (DOCENT.GT.0.0) XSEP = ZINC / 2.0
C
      DO 30 I = 1,NZI,ZINC
         IF (SBUFF(I).EQ.FBLANK) THEN
            PENUP = .TRUE.
            CTEMP = '   undefined   '
         ELSE
            Y = MAX (PIXR(1), MIN (PIXR(2), SBUFF(I))) * SCALY + OFY
            X = MAX (PBLC(1), I-XSEP)
            IF (ISFQID) THEN
               DVAL = (FRQS(I) - RVAL) / INCR + 1.0D0
               X = DVAL - XSEP
               X = MAX (PBLC(1), X)
               END IF
            IF (PENUP) THEN
               CALL GPOS (X, Y, PLBUFF, IRET)
               PENUP = .FALSE.
            ELSE
               CALL GVEC (X, Y, PLBUFF, IRET)
               END IF
            IF (IRET.NE.0) GO TO 980
            IF (DOCENT.GT.0.0) THEN
               X = MIN (PTRC(1), I+XSEP)
               IF (ISFQID) THEN
                  IF (I+ZINC.LE.NZI) THEN
                     DTEMP = (FRQS(I+ZINC)-RVAL) / INCR + 1.0D0
                     XSEP = (DTEMP - DVAL) / 2.0D0
                     END IF
                  X = DVAL + XSEP
                  X = MIN (PTRC(1), X)
                  END IF
               CALL GVEC (X, Y, PLBUFF, IRET)
               IF (IRET.NE.0) GO TO 980
               END IF
            END IF
C                                       For printer: fill LINE
         IF (PRINT) THEN
            ZPIX = NINT (BLC(3) + I - 1)
            ZVAL = (ZPIX - RPIX) * INCR + RVAL
            IF (ISFQID) ZVAL = FRQS(I)
            IF (INVERT) ZPIX = NINT (TRC(3) - I + 1)
C
            IF (DOWEIT) THEN
               WRITE (LINE, 1025) DBUFF(I)
               TITL1 = ' '
               TITL2 = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IRET)
            ELSE
               WRITE (LINE,1020) ZPIX, ZVAL, DBUFF(I), SBUFF(I)
               IF (DBUFF(I).EQ.FBLANK) LINE(26:40) = '  undefined  '
               IF (SBUFF(I).EQ.FBLANK) LINE(43:57) = '  undefined  '
               IF (.NOT.DOSMTH) LINE(43:) = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IRET)
               END IF
            PRINT = IRET.EQ.0
            END IF
 30      CONTINUE
C
      IF (DOWEIT) GO TO 970
C
      IF (XSUM.GT.0.0) THEN
         FSUM = FSUM / XSUM
         DSUM = DSUM / XSUM - FSUM * FSUM
         DSUM = SQRT (MAX (0.0, DSUM))
         END IF
      VMIN = FSUM - DSUM * VM(1)
      VMAX = FSUM + DSUM * VM(1)
      DO 50 L = 2,10
         SV = 0.
         SS = 0.
         NS = 0
         DO 40 I = 1,NZI
            V = DBUFF(I)
            IF ((V.NE.FBLANK) .AND. (V.GE.VMIN) .AND. (V.LE.VMAX)) THEN
               SV = SV + V
               SS = SS + V * V
               NS = NS + 1
               END IF
 40         CONTINUE
         IF (NS.GT.0) THEN
            SV = SV / NS
            SS = SS / NS - SV * SV
            SS = SQRT (MAX (0.0, SS))
            VMIN = SV - SS * VM(L)
            VMAX = SV + SS * VM(L)
         ELSE
            GO TO 55
            END IF
 50      CONTINUE
 55   IF (PRINT) THEN
         WRITE (LINE,1030) FSUM
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, IPCNT, PAGE, SCRTCH, IRET)
         WRITE (LINE,1031) DSUM
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, IPCNT, PAGE, SCRTCH, IRET)
         WRITE (LINE,1032) XSUM
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, IPCNT, PAGE, SCRTCH, IRET)
         IF (NS.GT.0) THEN
            WRITE (LINE,1033) SV
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IRET)
            WRITE (LINE,1034) SS
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IRET)
            WRITE (LINE,1035) NS
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IRET)
            END IF
      ELSE
         WRITE (MSGTXT,1030) FSUM
         CALL MSGWRT (5)
         WRITE (MSGTXT,1031) DSUM
         CALL MSGWRT (5)
         WRITE (MSGTXT,1032) XSUM
         CALL MSGWRT (5)
         IF (NS.GT.0) THEN
            WRITE (MSGTXT,1033) SV
            CALL MSGWRT (5)
            WRITE (MSGTXT,1034) SS
            CALL MSGWRT (5)
            WRITE (MSGTXT,1035) NS
            CALL MSGWRT (5)
            END IF
         END IF
C                                       end of WEIGHT
  970 CONTINUE
C                                       Finish up
      CALL GFINIS (PLBUFF, IRET)
      IF (.NOT.DOTV) CALL HIPLOT (DISKIN, CNOIN, PVER, BUFF1, IERR)
      GO TO 999
C                                       Plot error - try partial
 980  WRITE (MSGTXT,1980) IRET
      CALL MSGWRT (7)
      CALL GFINIS (PLBUFF, IERR)
      IF (IERR.EQ.0) THEN
         IRET = 0
      ELSE
         IF (.NOT.DOTV) THEN
            CALL ZCLOSE (GLUN, GFIND, IERR)
            CALL ZDESTR (DISKIN, PLNAME, IERR)
            END IF
         END IF
 985  IF ((IRET.NE.0) .AND. (.NOT.DOTV)) CALL DELEXT ('PL', DISKIN,
     *   CNOIN, 'READ', CATBLK, PLBUFF, PVER, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Created plot file version ', I4)
 1001 FORMAT ('PLTSPC: MADDEX ERROR ',I3)
 1005 FORMAT ('PLTSPC: ZPHFIL ERROR ',I3)
 1008 FORMAT (A,' over area in X:',2I5,'   in Y:',2I5)
 1009 FORMAT ('Freq smoothing parms:',F3.0,F5.2,F5.1)
 1010 FORMAT ('PLTSPC: GINIT ERROR ',I3)
 1015 FORMAT ('Plot file version',I4,'__created ',A12,A8)
 1020 FORMAT (I5,2X,1PE16.8,2(2X,1PE15.7))
 1025 FORMAT (F4.0)
 1030 FORMAT ('Average of all points   ',1PE15.7)
 1031 FORMAT ('RMS of all points       ',1PE15.7)
 1032 FORMAT ('Number points included  ',F8.0)
 1033 FORMAT ('Robust average of points',1PE15.7)
 1034 FORMAT ('Robust rms of points    ',1PE15.7)
 1035 FORMAT ('Number points included  ',I7)
 1980 FORMAT ('ERROR ',I5,' PLOTTING - TRY TO FINISH PARTIAL GRAPH')
      END
      SUBROUTINE SETSM (NCH, MAXSMO, SMOOTH, DOSMTH, SMTAB)
C-----------------------------------------------------------------------
C   SETSM determines the type of spectral smoothing to be applied and
C   sets up the look up table to do it. The actual smoothing is done in
C   routine SMOSP
C   Inputs:
C      NCH      I      Number samples on axis being smoothed
C      MAXSMO   I      Dimension of SMTAB
C      SMOOTH   R(3)   Array containing smoothing parms
C                         SMOOTH(1) = type of function
C                               (2) = width of function in channels
C                               (3) = support of function in channels
C                         Type of function supported are:
C                            0 => no smoothing
C                            1 => hanning
C                            2 => gaussian
C                            3 => boxcar
C                            4 => sin(x)/x
C   Output:
C      DOSMTH   L      T => do smoothing
C-----------------------------------------------------------------------
      INTEGER   NCH, MAXSMO
      REAL      SMOOTH(3), SMTAB(*)
      LOGICAL   DOSMTH
C
      INTEGER   I, N, LSPECT, IROUND, IT, SUPRAD
      REAL      FX, X, W, WIDTHS(4), SUPS(4)
      DATA WIDTHS /4.0, 2.0, 2.0, 3.0/
      DATA SUPS /1.0, 3.0, 1.0, 4.0/
C-----------------------------------------------------------------------
      DOSMTH = .FALSE.
      IT = IROUND (SMOOTH(1))
      IF (IT.LE.0) GO TO 999
      DOSMTH = .TRUE.
C                                       Convolution: parms & tables
      IF (IT.GT.4) IT = IT - 4
      IF (IT.GT.4) IT = 1
      SMOOTH(1) = IT
      LSPECT = MAX (12, NCH)
      IF ((SMOOTH(2).LT.0.5) .OR. (SMOOTH(2).GT.LSPECT/3.))
     *   SMOOTH(2) = WIDTHS(IT)
      IF ((SMOOTH(3).GT.4.*SUPS(IT)*SMOOTH(2)) .OR.
     *   (SMOOTH(3).LT.SMOOTH(2)))SMOOTH(3) = SUPS(IT) * SMOOTH(2)
      SUPRAD = SMOOTH(3) / 2.0 + 0.1
      IF (SUPRAD+1.GT.MAXSMO) THEN
         SUPRAD = MAXSMO - 1
         SMOOTH(2) = (2. * SUPRAD) / SUPS(IT)
         END IF
      SMOOTH(3) = 2.0 * SUPRAD + 1.0
      CALL RFILL (MAXSMO, 0.0, SMTAB)
      N = 1 + SUPRAD
      FX = 2.0 / SMOOTH(2)
      SMTAB(1) = 1.0
C                                       Compute look-up tables
      W = SMTAB(1)
C                                       Hanning smooth
      IF (IT.EQ.1) THEN
         DO 20 I = 2,N
            X = I - 1.0
            SMTAB(I) = MAX (0.0, 1.0-FX*X)
            W = W + 2 * SMTAB(I)
 20         CONTINUE
C                                       Gaussian smooth
      ELSE IF (IT.EQ.2) THEN
         FX = -LOG(2.0) * FX * FX
         DO 30 I = 2,N
            X = I - 1.0
            SMTAB(I) = EXP (FX * X * X)
            W = W + 2 * SMTAB(I)
 30         CONTINUE
C                                       Boxcar smooth
      ELSE IF (IT.EQ.3) THEN
         FX = 1.0 / FX
         DO 40 I = 2,N
            X = I - 1.0
            IF (X.LT.FX) THEN
               SMTAB(I) = 1.0
            ELSE IF (X.EQ.FX) THEN
               SMTAB(I) = 0.5
               END IF
            W = W + 2 * SMTAB(I)
 40         CONTINUE
C                                      Sinc smooth
      ELSE IF (IT.EQ.4) THEN
         FX = 3.14159 * FX
         DO 50 I = 2,N
            X = (I - 1.0) * FX
            SMTAB(I) = SIN(X) / X
            W = W + 2 * SMTAB(I)
 50         CONTINUE
         END IF
C                                       Normalize integral
      IF (W.LE.0.0) W = 1.0
      DO 70 I = 1,N
         SMTAB(I) = SMTAB(I) / W
 70      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SMOSP (NCH, SMOOTH, SMTAB, RANGE, SPCI, SPCO)
C-----------------------------------------------------------------------
C   SMOSP convolves an input spectrum with a convolving look up table
C   established in common.
C   Values from commons:
C      SMTAB    R(MAXSMO)   Convolution look-up table
C      SMOOTH   R(3)        (3) = 2*support-radius + 1
C      BCHANS   I           Start channel for smoothing
C      ECHANS   I           Stop channel for smoothing
C      SPCI     R(*)        Spectrum
C   Output:
C      RANGE    R(2)        new intensity range
C      SPCO     R(*)        Spectrum smoothed
C-----------------------------------------------------------------------
      REAL      SMOOTH(3), SMTAB(*), RANGE(2), SPCI(*), SPCO(*)
      INTEGER   NCH
C
      INTEGER   J, J1, J2, L, IFRQ, SUPRAD
      REAL      S, W
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SUPRAD = (SMOOTH(3) - 0.9) / 2.0
      RANGE(1) = 1.E20
      RANGE(2) = -1.E20
C                                       Convolve the data
      DO 30 IFRQ = 1,NCH
         J1 = MAX (IFRQ - SUPRAD, 1)
         J2 = MIN (IFRQ + SUPRAD, NCH)
         S = 0.0
         W = 0.0
         DO 20 J = J1,J2
            IF (SPCI(J).NE.FBLANK) THEN
               L = ABS(IFRQ-J) + 1
               S = SPCI(J) * SMTAB(L) + S
               W = SMTAB(L) + W
               END IF
 20         CONTINUE
         IF (W.GT.0.0) THEN
            SPCO(IFRQ) = S / W
            RANGE(1) = MIN (RANGE(1), SPCO(IFRQ))
            RANGE(2) = MAX (RANGE(2), SPCO(IFRQ))
         ELSE
            SPCO(IFRQ) = FBLANK
            END IF
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SLISPC (IRET)
C-----------------------------------------------------------------------
C   SLISPC writes the fit spectrum as a SL file
C   Output:
C      IRET   I   > 0 => serious error
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'RSPEC.INC'
      CHARACTER SFILE*48
      INTEGER   ISLDAT(512), IVER, NREC, SLUN, SIND, LREC, IPT, IERR,
     *   LNZI, NG(MAXIMG), NB(MAXIMG), IN, OUT
      REAL      RSLDAT(512), RMIN, RMAX, FQFINC
      DOUBLE PRECISION DSLDAT(256), FR, FW, F
      REAL      LBUFF(MAXIMG), SB(MAXIMG), WT(MAXIMG), W
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (ISLDAT, RSLDAT, DSLDAT)
      DATA SLUN /45/
C-----------------------------------------------------------------------
      CALL FNDEXT ('SL', CATBLK, IVER)
      IVER = IVER + 1
      CALL FILL (256, 0, ISLDAT)
      LREC = 256
      LNZI = NZI
      IF (ISFQID) LNZI = 4 * NZI - 3
      NREC = (LNZI - 1) / 256 + 2
C                                       create
      CALL EXTINI ('WRIT', 'SL', DISKIN, CNOIN, IVER, CATBLK, SLUN,
     *   SIND, LREC, NREC, ISLDAT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATE', IVER
         GO TO 995
         END IF
C                                       update record 1
      CALL ZFIO ('READ', SLUN, SIND, 1, ISLDAT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ 1', IVER
         GO TO 990
         END IF
      ISLDAT(57) = LNZI
      ISLDAT(58) = 0
      ISLDAT(59) = ISLDAT(1) + 1
      CALL ZFIO ('WRIT', SLUN, SIND, 1, ISLDAT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE 1', IVER
         GO TO 990
         END IF
C                                       inputs/parms in 2nd record
      CALL FILL (256, 0, ISLDAT)
      CALL CHR2H (6, TSKNAM, 1, ISLDAT(1))
      CALL ZDATE (ISLDAT(4))
      CALL ZTIME (ISLDAT(7))
      ISLDAT(10) = 23
      IF (INVERT) THEN
         RMIN = BLC(3)
         RMAX = TRC(3)
         BLC(3) = RMAX
         TRC(3) = RMIN
         END IF
      CALL RCOPY (23, USERID, RSLDAT(11))
      IF (INVERT) THEN
         BLC(3) = RMIN
         TRC(3) = RMAX
         END IF
C                                       local work buffer
      CALL RCOPY (NZI, DBUFF, LBUFF)
C                                       FQID?
      IF (ISFQID) THEN
         DSLDAT(19) = FRQS(1)
         FQFINC = (FRQS(NZI) - FRQS(1)) / (LNZI - 1)
         RSLDAT(36) = FQFINC
         END IF
C                                       FQID interpolate
      IF (ISFQID) THEN
         CALL RFILL (LNZI, 0.0, SB)
         CALL RFILL (LNZI, 0.0, WT)
         CALL FILL (LNZI, 0, NG)
         CALL FILL (LNZI, 0, NB)
         FW = 2.0D0 * FQFINC
C                                       convolve
         DO 30 IN = 1,NZI
            FR = FRQS(IN)
            F = FRQS(1) - FQFINC
            DO 20 OUT = 1,LNZI
               F = F + FQFINC
               W = ((FR - F) / FW) ** 2
               IF (W.LT.10.D0) THEN
                  IF (LBUFF(IN).NE.FBLANK) THEN
                     W = EXP(-W)
                     WT(OUT) = WT(OUT) + W
                     SB(OUT) = SB(OUT) + W * LBUFF(IN)
                     NG(OUT) = NG(OUT) + 1
                  ELSE
                     NB(OUT) = NB(OUT) + 2
                     END IF
                  END IF
 20            CONTINUE
 30         CONTINUE
C                                       average
         DO 40 OUT = 1,LNZI
            IF ((WT(OUT).GT.0.0) .AND. (NB(OUT).LT.NG(OUT))) THEN
               LBUFF(OUT) = SB(OUT) / WT(OUT)
            ELSE
               LBUFF(OUT) = FBLANK
               END IF
 40         CONTINUE
         END IF
C                                       min/max
      RMIN = 1.E12
      RMAX = -RMIN
      DO 50 IPT = 1,LNZI
         IF (DBUFF(IPT).NE.FBLANK) THEN
            RMAX = MAX (RMAX, LBUFF(IPT))
            RMIN = MIN (RMIN, LBUFF(IPT))
            END IF
 50      CONTINUE
      RSLDAT(34) = RMIN
      RSLDAT(35) = RMAX
      CALL ZFIO ('WRIT', SLUN, SIND, 2, ISLDAT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE 2', IVER
         GO TO 990
         END IF
C                                       write the data
      NREC = 2
      IPT = 1
 100  LREC = LNZI + 1 - IPT
      IF (LREC.GT.0) THEN
         LREC = MIN (LREC, 256)
         CALL RCOPY (LREC, LBUFF(IPT), RSLDAT)
         NREC = NREC + 1
         CALL ZFIO ('WRIT', SLUN, SIND, NREC, ISLDAT, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE DATA', IVER
            GO TO 990
            END IF
         IPT = IPT + LREC
         IF (IPT.LE.LNZI) GO TO 100
         END IF
C                                       close
      CALL ZCLOSE (SLUN, SIND, IRET)
C                                       Slice file created message.
      WRITE (MSGTXT,1020) IVER
      CALL MSGWRT (3)
      GO TO 999
C                                       destroy SL file
 990  CALL MSGWRT (8)
      CALL ZCLOSE (SLUN, SIND, IERR)
      CALL ZPHFIL ('SL', DISKIN, CNOIN, IVER, SFILE, IERR)
      CALL ZDESTR (DISKIN, SFILE, IERR)
      CALL DELEXT ('SL', DISKIN, CNOIN, 'WRIT', CATBLK, ISLDAT, IVER,
     *   IERR)
      GO TO 999
C
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' ON ',A,' SL VERS',I5)
 1020 FORMAT ('SLice file version ',I5,' created.')
      END
