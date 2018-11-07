LOCAL INCLUDE 'TRANS.INC'
      CHARACTER INNAME*12, INCLAS*6, OUTNAM*12, OUTCLS*6, OPCODE*14
      HOLLERITH XINNAM(3), XINCLA(2), XOUTNA(3), XOUTCL(2), XOPCOD(4)
      REAL      INSEQ, INDISK, OUTSEQ, OUTDSK, BLC(7), TRC(7),
     * BADISK(10)
      REAL      RMAX, RMIN
      INTEGER   RIAX(7), RSGN(7), NPI(7), NSI(7), DONMAX, INLUN, ININD,
     *   OUTLUN, OUTIND, WORK(512)
C
      COMMON /INPARM/ XINNAM, XINCLA, INSEQ, INDISK, XOUTNA, XOUTCL,
     *   OUTSEQ, OUTDSK, BLC, TRC, XOPCOD, BADISK
      COMMON /CHPARM/ INNAME, INCLAS, OUTNAM, OUTCLS, OPCODE
      COMMON /TRACOM/ RMIN, RMAX, DONMAX, RIAX, RSGN,
     *   NPI, NSI, INLUN, ININD, OUTLUN, OUTIND
      COMMON /TRABUF/ WORK
LOCAL END
      PROGRAM TRANS
C-----------------------------------------------------------------------
C! Generalized 7-D matrix transposition program
C# Map-util Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2005, 2008-2010, 2012-2013, 2016
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
C   TRANS is a generalized 7-dimensional matrix transposition program
C   which runs under control of AIPS.
C   Adverbs: INNAME    R(3)    Input image: name 12 packed chars
C            INCLASS   R(2)               : class 6 packed chars
C            INSEQ     R                  : seq #
C            INDISK    R                  : disk number
C            OUTNAME   R(3)    Ouput image: name 12 packed chars
C            OUTCLASS  R(2)               : class 6 packed chars
C            OUTSEQ    R                  : seq # , 0 => low unique
C            OUTDISK   R                  : disk #, 0 => INDISK
C            BLC       R(7)    Subimage input: bottom left corner
C            TRC       R(7)                  : top right corner
C            TRACODE   R(4)    New axis order in terms of old axis
C                              numbers: <= 14 packed chars.
C            BADDISK   R(10)   List of disks to avoid for scratch files
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   RSC1, RSC2
      PARAMETER (RSC1 = 4096)
      PARAMETER (RSC2 = 64)
C
      CHARACTER PHNAME*48
      INTEGER   IAX(7), IERR, JJ, KVOL, NN, BYTPIX, NPST(7), I, IRET
      LOGICAL   T, DONE
      LONGINT   PBUF1
      INTEGER   NBUF1, NBUF2
      REAL      RBUF2(RSC2*MAXIMG), RBUF1(2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'TRANS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       initialization, get parms
C                                       create output map
      CALL TRAINI (IERR)
      IF (IERR.NE.0) GO TO 900
C                                       get big memory
      NBUF2 = MAXIMG * RSC2 * 2
      NBUF1 = (MAXIMG * RSC1 - 1) / 1024 + 1
      NBUF1 = MIN (NBUF1, KAPWRD)
      CALL ZMEMRY ('GET ', TSKNAM, NBUF1, RBUF1, PBUF1, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'COULD NOT GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         GO TO 990
         END IF
      NBUF1 = 2048 * NBUF1
      JJ = RIAX(1)
      KVOL = FVOL(1)
      BYTPIX = 2
C                                       Short rows w subimage or swap
      IF (NSI(1)*BYTPIX.GT.NBPS/2) GO TO 100
         DO 20 I = 1,7
            IF ((RSGN(I).LT.0) .OR. (NSI(I).LT.NPI(I))) GO TO 25
 20         CONTINUE
         GO TO 100
C                                       Do simple subarray op.
 25      DONE = .TRUE.
         DO 30 I = 1,7
            IAX(I) = I
            DONE = (DONE) .AND. (IAX(I).EQ.RIAX(I))
 30         CONTINUE
         OUTLUN = INLUN + 1
C                                       Create scratch
         IF (DONE) GO TO 35
            CALL MAPSNC (CATBLK(KIDIM), NSI, KVOL, OUTLUN, OUTIND, WORK,
     *         IERR)
            IF (IERR.NE.0) GO TO 900
            KVOL = SCRVOL(NSCR)
            GO TO 40
C                                       Open final output file
 35      CONTINUE
            CALL ZPHFIL ('MA', FVOL(2), FCNO(2), 1, PHNAME, IERR)
            CALL ZOPEN (OUTLUN, OUTIND, FVOL(2), PHNAME, T, T, T, IERR)
            IF (IERR.EQ.0) GO TO 40
               WRITE (MSGTXT,1035) IERR
               CALL MSGWRT (8)
               GO TO 900
C                                       Do it finally
 40      CALL TRAROW (IAX, NBUF1, NBUF2, RBUF1(1+PBUF1), RBUF2, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL ZCLOSE (INLUN, ININD, IERR)
C                                       Pass along scratch file
         IF (DONE) GO TO 400
            INLUN = OUTLUN
            ININD = OUTIND
            CALL FILL (7, 1, RSGN)
            CALL COPY (7, NSI, NPI)
C                                       X axis is involved!
 100  IF (JJ.EQ.1) GO TO 300
C                                       prepare submatrix for X x Y
         IF ((JJ.EQ.2) .AND. (RSGN(1).GT.0) .AND. (RSGN(2).GT.0)
     *      .AND. (DONMAX.GT.0)) GO TO 200
C                                       Axes to do this time: mostly
C                                       no movement, swap JJ with 2
            DO 120 I = 1,7
               IAX(I) = I
               NPST(I) = NSI(I)
 120           CONTINUE
            NPST(2) = NSI(JJ)
            NPST(JJ) = NSI(2)
            IAX(2) = JJ
            IAX(JJ) = 2
C                                       Create, open scratch file
            OUTLUN = INLUN + 1
            CALL MAPSNC (CATBLK(KIDIM), NPST, KVOL, OUTLUN, OUTIND,
     *         WORK, IERR)
            IF (IERR.NE.0) GO TO 900
            KVOL = SCRVOL(NSCR)
C                                       Read-scatter write: apply
C                                       subim, find max/min
            CALL TRAROW (IAX, NBUF1, NBUF2, RBUF1(1+PBUF1), RBUF2, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL ZCLOSE (INLUN, ININD, IERR)
C                                       pass along new input file
            INLUN = OUTLUN
            ININD = OUTIND
C                                       update tables to reflect change
            CALL FILL (7, 1, RSGN)
            CALL COPY (7, NSI, NPI)
            IF (JJ.LE.2) GO TO 200
               DO 140 I = 1,7
                  IF (RIAX(I).NE.JJ) GO TO 130
                     RIAX(I) = 2
                     NPI(2) = NSI(JJ)
                     GO TO 140
 130              IF (RIAX(I).NE.2) GO TO 140
                     RIAX(I) = JJ
                     NPI(JJ) = NSI(2)
 140              CONTINUE
C                                       X with Y transpose
C                                       this is not last step
 200     IF (RIAX(2).EQ.1) GO TO 220
            DO 210 I = 3,7
               IAX(I) = I
 210           CONTINUE
            IAX(1) = 2
            IAX(2) = 1
            NN = NPI(2)
            NPI(2) = NPI(1)
            NPI(1) = NN
            OUTLUN = INLUN + 1
            CALL MAPSNC (CATBLK(KIDIM), NPI, KVOL, OUTLUN, OUTIND, WORK,
     *         IERR)
            IF (IERR.NE.0) GO TO 900
            NPI(1) = NPI(2)
            NPI(2) = NN
            KVOL = SCRVOL(NSCR)
            GO TO 230
C                                       This is the last step
 220     CONTINUE
            CALL COPY (7, RIAX, IAX)
            OUTLUN = INLUN + 1
C                                       Open output map as out file
            CALL ZPHFIL ('MA', FVOL(2), FCNO(2), 1, PHNAME, IERR)
            CALL ZOPEN (OUTLUN, OUTIND, FVOL(2), PHNAME, T, T, T, IERR)
            IF (IERR.EQ.0) GO TO 230
               WRITE (MSGTXT,1035) IERR
               CALL MSGWRT (8)
               GO TO 900
C                                       Do X with Y
C                                       swap which is large buffer
 230     CALL TRAXPO (IAX, NBUF2, NBUF1, RBUF2, RBUF1(1+PBUF1), IERR)
         IF (IERR.NE.0) GO TO 900
         CALL ZCLOSE (INLUN, ININD, IERR)
C                                       Pass along file if not done
         IF (RIAX(2).EQ.1) GO TO 400
            INLUN = OUTLUN
            ININD = OUTIND
C                                       Bring parms up to date
            DO 240 I = 1,7
               RSGN(I) = 1
               IF (RIAX(I).NE.2) GO TO 235
                  RIAX(I) = 1
                  GO TO 240
 235           IF (RIAX(I).EQ.1) RIAX(I) = 2
 240           CONTINUE
            NN = NPI(2)
            NPI(2) = NPI(1)
            NPI(1) = NN
C                                       Final read-scatter write
 300  OUTLUN = INLUN + 1
      CALL ZPHFIL ('MA', FVOL(2), FCNO(2), 1, PHNAME, IERR)
      CALL ZOPEN (OUTLUN, OUTIND, FVOL(2), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       Do it
      CALL TRAROW (RIAX, NBUF1, NBUF2, RBUF1(1+PBUF1), RBUF2, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Final header update...
 400  IRET = 0
      IF (DONMAX.NE.2) THEN
         CATR(KRDMX) = RMAX
         CATR(KRDMN) = RMIN
         CALL CATIO ('UPDT', FVOL(2), FCNO(2), CATBLK, 'REST',
     *      WORK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1400) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      GO TO 990
C                                       ERRORS: close files
 900  IRET = 8
C                                       Clear files, restart AIPS, etc
 990  CALL DIE (IRET, WORK)
C
 999  STOP
C-----------------------------------------------------------------------
 1035 FORMAT ('UNABLE TO OPEN OUTPUT FILE: IERR=',I7)
 1400 FORMAT ('WARNING: MAX/MIN NOT UPDATED IN HEADER: IERR=',I7)
      END
      SUBROUTINE TRAINI (IERR)
C-----------------------------------------------------------------------
C   TRAINI performs initializations for program TRANS: init basic
C   commons, open input map, create output map (including HI file),
C   and interpret transposition code.
C   Output: IERR   I       0 => ok, rest => quit
C   Output commons:
C      /MAPHDR/ new map header
C      /CFILES/ file handling for die: init, enter catlgd files
C      /TRACOM/ task parms
C      /INPARM/ input parms with defaults filled in
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, NUMBER(7)*1, FCHARS(3)*4, CTEMP*8, MTYPE*2,
     *   IT*1, NOTTYP*2
      INTEGER   NPARMS, IERR, IRET, IER, IUSER, S2B(256), IS, ITMP,
     *   LUNH1, LUNH2, I, INC, ISEQ,  J, K, L, M, NAX, IROUND, NONOT,
     *   NAXP(7)
      REAL      S4B(128)
      DOUBLE PRECISION    S8B(64)
      LOGICAL   T, EQUAL
      INCLUDE 'TRANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (S2B, S4B, S8B)
      DATA NPARMS, PRGNAM / 42, 'TRANS '/
      DATA NUMBER /'1','2','3','4','5','6','7'/
      DATA LUNH1, LUNH2 /27, 28/
      DATA T /.TRUE./
      DATA FCHARS /'FREQ','VELO','FELO'/
      DATA NONOT, NOTTYP /0, '  '/
C-----------------------------------------------------------------------
C                                       File init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       Get parms from AIPS
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XINNAM, WORK, IERR)
      IRET = IERR
      IF (IRET.NE.0) THEN
         IF (IRET.EQ.1) GO TO 999
            WRITE (MSGTXT,1000)
            CALL MSGWRT (9)
         END IF
      IF (RQUICK) CALL RELPOP (IRET, WORK, IER)
      IF (IRET.NE.0) GO TO 999
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XINNAM, INNAME)
      CALL H2CHR (6, 1, XINCLA, INCLAS)
      CALL H2CHR (12, 1, XOUTNA, OUTNAM)
      CALL H2CHR (6, 1, XOUTCL, OUTCLS)
      CALL H2CHR (14, 1, XOPCOD, OPCODE)
C                                       Open input map
      FVOL(1) = IROUND (INDISK)
      FRW(1) = 0
      ISEQ = IROUND (INSEQ)
      IUSER = NLUSER
      INLUN = 16
      MTYPE = 'MA'
      CALL MAPOPN ('READ', FVOL, INNAME, INCLAS, ISEQ, MTYPE, IUSER,
     *   INLUN, ININD, FCNO, CATBLK, WORK, IERR)
      IF (IERR.GT.1) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         GO TO 999
         END IF
      NCFILE = 1
C                                       Check/set window
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Fill in defaults
      IF (OUTDSK.GT.NVOL) OUTDSK = FVOL(1)
      I = IROUND (OUTSEQ)
      CALL MAKOUT (INNAME, INCLAS, ISEQ, '      ', OUTNAM, OUTCLS, I)
      OUTSEQ = I
      DO 25 I = 1,10
         IBAD(I) = IROUND (BADISK(I))
 25      CONTINUE
C                                       Default transpose, set subim
      DONMAX = 2
      DO 30 I = 1,7
         RIAX(I) = I
         RSGN(I) = 1
         NPI(I) = MAX (1, CATBLK(KINAX+I-1))
         NSI(I) = TRC(I) - BLC(I) + 1.01
         IF (NSI(I).LT.NPI(I)) DONMAX = 0
         NAXP(I) = NSI(I)
         IF (NSI(I).LE.1) NAXP(I) = 0
 30      CONTINUE
C                                       What transpose requested
      J = 0
      M = 0
      DO 55 I = 1,7
         IS = 1
         J = J + 1
         IT = OPCODE(J:J)
         IF ((IT.NE.' ') .AND. (IT.NE.',')) THEN
            IF (IT.EQ.'-') THEN
               IS = -1
               J = J + 1
               IT = OPCODE(J:J)
               END IF
            DO 45 K = 1,7
               IF (IT.EQ.NUMBER(K)) THEN
C                                       check for duplicate
                  DO 40 L = 1,M
                     IF (RIAX(L).EQ.K) GO TO 900
 40                  CONTINUE
                  M = M + 1
                  RIAX(M) = K
                  RSGN(K) = IS
                  NAXP(K) = 0
                  GO TO 55
                  END IF
 45            CONTINUE
            GO TO 900
            END IF
 55      CONTINUE
      J = 0
      DO 60 I = 1,7
         J = J + NAXP(I)
 60      CONTINUE
      IF (J.GT.0) THEN
         MSGTXT = 'YOU DID NOT SPECIFY ALL REAL AXES'
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       max/min setup
      IF (DONMAX.NE.2) THEN
         RMAX = CATR(KRDMN)
         RMIN = CATR(KRDMX)
         END IF
C                                       build new header
C                                       WARNING: this may be too naive
C                                       re coord increments, rotation
      CALL COPY (256, CATBLK, S2B)
      NAX = S2B(KIDIM)
      INC = 2
      DO 110 I = 1,NAX
         K = RIAX(I)
         CATBLK(KINAX+I-1) = NSI(K)
         ITMP = KHCTP+(K-1)*INC
         CALL H2CHR (8, 1, S4B(ITMP), CTEMP)
         ITMP = KHCTP+(I-1)*INC
         CALL CHR2H (8, CTEMP, 1, CATH(ITMP))
         CATD(KDCRV+I-1) = S8B(KDCRV+K-1)
         CATR(KRCIC+I-1) = S4B(KRCIC+K-1) * RSGN(K)
         CATR(KRCRT+I-1) = S4B(KRCRT+K-1)
         CATR(KRCRP+I-1) = S4B(KRCRP+K-1) - BLC(K) + 1.0
         IF (RSGN(K).LT.0) CATR(KRCRP+I-1) = TRC(K) - S4B(KRCRP+K-1)
     *      + 1.0
         IF (CATBLK(KIALT).NE.0) THEN
            EQUAL = .FALSE.
            ITMP = KHCTP+(K-1)*INC
            CALL H2CHR (4, 1, S4B(ITMP), CTEMP)
            DO 105 J = 1,3
               IF (.NOT.EQUAL) EQUAL = FCHARS(J).EQ.CTEMP(:4)
 105           CONTINUE
            IF (EQUAL) THEN
               IF (RSGN(K).LT.0) THEN
                  CATR(KRARP) = TRC(K) - CATR(KRARP) + 1.0
               ELSE
                  CATR(KRARP) = CATR(KRARP) - BLC(K) + 1.0
                  END IF
               END IF
            END IF
 110     CONTINUE
C                                       create the output map
      CATBLK(KIIMS) = OUTSEQ + 0.01
      FVOL(2) = IROUND (OUTDSK)
      FRW(2) = 2
      CALL CHR2H (12, OUTNAM, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, OUTCLS, KHIMCO, CATH(KHIMC))
      CALL MCREAT (FVOL(2), FCNO(2), WORK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110)
         CALL MSGWRT (8)
         GO TO 999
         END IF
      NCFILE = 2
C                                       copy keywords
      CALL KEYCOP (FVOL(1), FCNO(1), FVOL(2), FCNO(2), IERR)
C                                       History file create & add
      CALL HIINIT (3)
      CALL HISCOP (LUNH1, LUNH2, FVOL(1), FVOL(2), FCNO(1), FCNO(2),
     *   CATBLK, WORK(257), WORK, IERR)
      IF (IERR.GE.3) GO TO 130
      CALL HENCO1 (PRGNAM, INNAME, INCLAS, ISEQ, FVOL(1), LUNH2,
     *   WORK, IERR)
      IF (IERR.NE.0) GO TO 130
      CALL HENCOO (PRGNAM, OUTNAM, OUTCLS, CATBLK(KIIMS), FVOL(2),
     *   LUNH2, WORK, IERR)
      IF (IERR.NE.0) GO TO 130
      WRITE (MSGTXT,1120) PRGNAM, BLC
      CALL HIADD (LUNH2, MSGTXT, WORK, IERR)
      IF (IERR.NE.0) GO TO 130
      WRITE (MSGTXT,1121) PRGNAM, TRC
      CALL HIADD (LUNH2, MSGTXT, WORK, IERR)
      IF (IERR.NE.0) GO TO 130
      WRITE (MSGTXT,1122) PRGNAM, OPCODE
      CALL HIADD (LUNH2, MSGTXT, WORK, IERR)
 130  CALL HICLOS (LUNH2, T, WORK, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUNH1, LUNH2, FVOL(1), FVOL(2),
     *   FCNO(1), FCNO(2), CATBLK, WORK(257), WORK, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
      IERR = 0
      GO TO 999
C                                       Error in parse
 900  WRITE (MSGTXT,1900) OPCODE
      CALL MSGWRT (8)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FAILS TO GET PARMS FROM AIPS')
 1010 FORMAT ('FAILS TO FIND AND OPEN INPUT IMAGE')
 1110 FORMAT ('FAILS TO CREATE OUTPUT IMAGE')
 1120 FORMAT (A6,'BLC= ',6(F5.0,','),F5.0,' / Input image')
 1121 FORMAT (A6,'TRC= ',6(F5.0,','),F5.0,' / Input image')
 1122 FORMAT (A6,'TRANCOD=''',A14,''' / Output axis order')
 1900 FORMAT ('IMPROPER TRANSPOSITION CODE ',A14)
      END
      SUBROUTINE TRAROW (IAX, NBUF1, NBUF2, RBUF1, RBUF2, IERR)
C-----------------------------------------------------------------------
C   TRAROW transposes axes 2, 3, 4, 5, 6, and 7 with each other
C   performing sub-imaging, order reversal, etc. on the way.
C   An especially difficult swap is one in which there is more than 1
C   row/sector and axis 2 is switching with axes 3, 4, 5, 6, or 7.
C   For this case, the routine assumes that any subimaging, order
C   reversal, etc has already been done.  Note this well!!!!!
C   Input:
C      IAX    I(7)   IAX(out axis #) = in axis # (n.b. IAX(1)=1
C                       required by this subroutine)
C   Output:
C      IERR   I      Error code: > 0 => quit
C   Common /TRACOM/ mostly:
C      DONMAX  >0 => max/min known, use NPI as input/output img size
C              =0 => max/min must be found.  Use NPI as input size,
C                    NSI as output size, & use BLC,TRC on the input
C      NPI(7) # points on input axes 1 - 7
C      NSI(7) # points desired on input axes 1 - 7 = (TRC-BLC+1)
C      INLUN, ININD   input file already open
C      OUTLUN, OUTIND  output file already open
C   Only DONMAX is alterred by this subroutine.
C-----------------------------------------------------------------------
      INTEGER   IAX(7), NBUF1, NBUF2, IERR
      REAL      RBUF1(*), RBUF2(*)
C
      INTEGER   JAX(7), IS(7), IOF(7), IWIN(4), IDEPTH(5), JDEPTH(5),
     *   IXL, I3A, I3B, I4A, I4B, I5A, I5B, I6A, I6B, I7A, I7B, I3, I4,
     *   I5, I6, I7, NX, NY, IB, NBYTES, BLKLX, IBLKOF, ISC, IRC, IDX,
     *   OWIN(4), IYL, NVPBUF, OBLKOF, NPST(7), OUTPOS, OBZERO, IBZERO,
     *   OUTPS0, I, I0, I2, IL, NYO, NYI, IYI, INBYTS, INPOS, IYI1, IYO,
     *   IYO1, J, NRIPS, NRIR, NS1, NS2, NV1, NV2, NVPS, NPL, LBYTES,
     *   JJO, JJ3, II3
      REAL      R
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TRANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA OWIN /4*0/
C-----------------------------------------------------------------------
C                                       Compute addressing pointers
      IDX = 0
C                                       Windows, more addr. pointers
      IF (DONMAX.LE.0) THEN
         IXL = 0
         I3A = BLC(3) + 0.01
         I4A = BLC(4) + 0.01
         I5A = BLC(5) + 0.01
         I6A = BLC(6) + 0.01
         I7A = BLC(7) + 0.01
         I3B = TRC(3) + 0.01
         I4B = TRC(4) + 0.01
         I5B = TRC(5) + 0.01
         I6B = TRC(6) + 0.01
         I7B = TRC(7) + 0.01
         IWIN(1) = BLC(1) + 0.01
         IWIN(2) = BLC(2) + 0.01
         IWIN(3) = TRC(1) + 0.01
         IWIN(4) = TRC(2) + 0.01
         DO 15 I = 1,7
            IF (IAX(I).NE.I) IXL = 1
            NPST(I) = NSI(IAX(I))
            JAX(IAX(I)) = I
            IS(I) = 1
            IOF(I) = -BLC(I) - 0.01
            IF (RSGN(I).GT.0) GO TO 15
               IS(I) = -1
               IOF(I) = IOF(I) + 1 - NSI(I)
 15         CONTINUE
         IF (IXL.EQ.0) THEN
            MSGTXT = 'Begin subimaging'
         ELSE
            MSGTXT = 'Begin row swap with subimaging'
            END IF
         CALL MSGWRT (2)
      ELSE
         MSGTXT = 'Begin row swap'
         CALL MSGWRT (2)
         I3A = 1
         I4A = 1
         I5A = 1
         I6A = 1
         I7A = 1
         I3B = MAX (1, NPI(3))
         I4B = MAX (1, NPI(4))
         I5B = MAX (1, NPI(5))
         I6B = MAX (1, NPI(6))
         I7B = MAX (1, NPI(7))
         IWIN(1) = 1
         IWIN(2) = 1
         IWIN(3) = NPI(1)
         IWIN(4) = NPI(2)
         DO 25 I = 1,7
            NPST(I) = NPI(IAX(I))
            JAX(IAX(I)) = I
            IS(I) = 1
            IOF(I) = -1
            IF (RSGN(I).LE.0) THEN
               IS(I) = -1
               IOF(I) = -NPI(I)
               END IF
 25         CONTINUE
         END IF
C                                       x-axis handling code
      IXL = 1
      IF (RSGN(1).LE.0) IXL = IXL + 1
      IF (DONMAX.LE.0) IXL = IXL + 2
      IB = 2
      NX = IWIN(3) - IWIN(1) + 1
      NY = IWIN(4) - IWIN(2) + 1
      NBYTES = NX * 2
      BLKLX = 1 + (NBYTES-1)/NBPS
      NBYTES = BLKLX * NBPS
C                                       Type of write to use
      IYL = 1
      IF (IAX(2).NE.2) IYL = 2
      IF ((NX*2.LE.NBPS/2) .AND. (IAX(2).NE.2)) GO TO 500
C                                       Read Y backwards?
      IF (RSGN(2).LE.0) THEN
         I = IWIN(2)
         IWIN(2) = IWIN(4)
         IWIN(4) = I
         END IF
      IF (IYL.GT.1) THEN
         NVPBUF = NBYTES / 2
         IRC = BLKLX
C                                       can we do multipl planes?
         NPL = (NBUF1 / 2) / (NVPBUF * NY)
      ELSE
         NPL = 1
         END IF
C                                       NO or plane swap
      IF ((NPL.LT.2) .OR. (IYL.LE.1)) THEN
C                                       READ LOOP: plane by plane
         DO 150 I7 = I7A,I7B
         DO 150 I6 = I6A,I6B
         DO 150 I5 = I5A,I5B
         DO 150 I4 = I4A,I4B
         DO 150 I3 = I3A,I3B
C                                       set up read op
            IDEPTH(1) = I3
            IDEPTH(2) = I4
            IDEPTH(3) = I5
            IDEPTH(4) = I6
            IDEPTH(5) = I7
            CALL COMOFF (CATBLK(KIDIM), NPI, IDEPTH, IBLKOF, IERR)
            IF (IERR.NE.0) GO TO 999
            IBLKOF = IBLKOF + 1
            CALL MINIT ('READ', INLUN, ININD, NPI(1), NPI(2), IWIN,
     *         RBUF2, NBUF2, IBLKOF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR, I3, I4
               GO TO 990
               END IF
C                                       prepare write parms
            DO 110 I = 3,7
               J = JAX(I) - 2
               IF (J.GT.0) JDEPTH(J) = (IDEPTH(I-2)+IOF(I))*IS(I) + 1
 110           CONTINUE
C                                       Use MINIT for plane swaps
            IF (IYL.LE.1) THEN
               CALL COMOFF (CATBLK(KIDIM), NPST, JDEPTH, OBLKOF, IERR)
               IF (IERR.NE.0) GO TO 999
               OBLKOF = OBLKOF + 1
               CALL MINIT ('WRIT', OUTLUN, OUTIND, NX, NY, OWIN, RBUF1,
     *            NBUF1, OBLKOF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1110) IERR
                  GO TO 990
                  END IF
               END IF
C                                       LOOP: over rows
            DO 145 I2 = 1,NY
C                                       read a row
               CALL MDISK ('READ', INLUN, ININD, RBUF2, INPOS, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1115) IERR
                  GO TO 990
                  END IF
C                                       plane swaps
               IF (IYL.EQ.1) THEN
                  CALL MDISK ('WRIT', OUTLUN, OUTIND, RBUF1, OUTPOS,
     *               IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1120) IERR
                     GO TO 990
                     END IF
C                                       last write done ?
               ELSE
                  IB = 3 - IB
                  IF (IDX.GE.2) THEN
                     CALL ZWAIT (OUTLUN, OUTIND, IB, IERR)
                     IF (IERR.NE.0) THEN
                        WRITE (MSGTXT,1120) IERR
                        GO TO 990
                        END IF
                     END IF
C                                       block to write
                  JDEPTH(JAX(2)-2) = I2
                  CALL COMOFF (CATBLK(KIDIM), NPST, JDEPTH, OBLKOF,
     *               IERR)
                  IF (IERR.NE.0) GO TO 999
                  IF (IDX.LT.10) IDX = IDX + 1
                  I = IAX(2)
                  ISC = (IDEPTH(I-2)+IOF(I))*IS(I)
                  OUTPOS = 1 + (IB-1) * NVPBUF
                  ISC = IRC * ISC
                  OBLKOF = OBLKOF + ISC
                  OBLKOF = OBLKOF + 1
                  END IF
C                                       move X row
C                                       real, forward
               IF (IXL.EQ.1) THEN
                  CALL RCOPY (NX, RBUF2(INPOS), RBUF1(OUTPOS))
C                                       real, backward
               ELSE IF (IXL.EQ.2) THEN
                  J = INPOS + NX
                  DO 115 I = 1,NX
                     RBUF1(OUTPOS+I-1) = RBUF2(J-I)
 115                 CONTINUE
C                                       real, forward
               ELSE IF (IXL.EQ.3) THEN
                  J = INPOS - 1
                  DO 120 I = 1,NX
                     R = RBUF2(J+I)
                     RBUF1(OUTPOS+I-1) = R
                     IF (R.NE.FBLANK) THEN
                        RMAX = MAX (R, RMAX)
                        RMIN = MIN (R, RMIN)
                        END IF
 120                 CONTINUE
C                                       real, backward
               ELSE IF (IXL.EQ.4) THEN
                  J = INPOS + NX
                  DO 125 I = 1,NX
                     R = RBUF2(J-I)
                     RBUF1(OUTPOS+I-1) = R
                     IF (R.NE.FBLANK) THEN
                        RMAX = MAX (R, RMAX)
                        RMIN = MIN (R, RMIN)
                        END IF
 125                 CONTINUE
                  END IF
C                                       Write the row
               IF (IYL.NE.1) THEN
                  CALL ZMIO ('WRIT', OUTLUN, OUTIND, OBLKOF, NBYTES,
     *               RBUF1(OUTPOS), IB, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1220) IERR
                     GO TO 990
                     END IF
                  END IF
 145           CONTINUE
            IF (IYL.EQ.1) THEN
               CALL MDISK ('FINI', OUTLUN, OUTIND, RBUF1, OUTPOS, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1120) IERR
                  GO TO 990
                  END IF
               END IF
 150        CONTINUE
C                                       Wait for all writes to finish
         IF (IYL.NE.1) THEN
            IB = 3 - IB
            CALL ZWAIT (OUTLUN, OUTIND, IB, IERR)
            IB = 3 - IB
            IF (IERR.EQ.0) CALL ZWAIT (OUTLUN, OUTIND, IB, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1250) IERR
               GO TO 990
               END IF
            END IF
C                                       READ LOOP: multiple planes
      ELSE
         DO 290 I7 = I7A,I7B
         DO 290 I6 = I6A,I6B
         DO 290 I5 = I5A,I5B
         DO 290 I4 = I4A,I4B
         DO 290 I3 = I3A,I3B,NPL
            JJ3 = I3 + NPL - 1
            JJ3 = MIN (JJ3, I3B)
            LBYTES = NBYTES * (JJ3 - I3 + 1)
            IDEPTH(1) = I3
            IDEPTH(2) = I4
            IDEPTH(3) = I5
            IDEPTH(4) = I6
            IDEPTH(5) = I7
            DO 250 II3 = I3,JJ3
C                                       set up read op
               IDEPTH(1) = II3
               CALL COMOFF (CATBLK(KIDIM), NPI, IDEPTH, IBLKOF, IERR)
               IF (IERR.NE.0) GO TO 999
               IBLKOF = IBLKOF + 1
               CALL MINIT ('READ', INLUN, ININD, NPI(1), NPI(2), IWIN,
     *            RBUF2, NBUF2, IBLKOF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1100) IERR, I3, I4
                  GO TO 990
                  END IF
C                                       prepare write parms
               DO 210 I = 3,7
                  J = JAX(I) - 2
                  IF (J.GT.0) JDEPTH(J) = (IDEPTH(I-2)+IOF(I))*IS(I) + 1
 210              CONTINUE
C                                       LOOP: over rows
               IF (RSGN(3).LE.0) THEN
                  JJO = (JJ3-II3) * NBYTES/2 + 1
               ELSE
                  JJO = (II3-I3) * NBYTES/2 + 1
                  END IF
               DO 245 I2 = 1,NY
C                                       read a row
                  CALL MDISK ('READ', INLUN, ININD, RBUF2, INPOS, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1115) IERR
                     GO TO 990
                     END IF
C                                       set outpos
                  OUTPOS = JJO + (I2-1) * LBYTES/2
C                                       move X row
C                                       real, forward
                  IF (IXL.EQ.1) THEN
                     CALL RCOPY (NX, RBUF2(INPOS), RBUF1(OUTPOS))
C                                       real, backward
                  ELSE IF (IXL.EQ.2) THEN
                     J = INPOS + NX
                     DO 220 I = 1,NX
                        RBUF1(OUTPOS+I-1) = RBUF2(J-I)
 220                    CONTINUE
C                                       real, forward
                  ELSE IF (IXL.EQ.3) THEN
                     J = INPOS - 1
                     DO 230 I = 1,NX
                        R = RBUF2(J+I)
                        RBUF1(OUTPOS+I-1) = R
                        IF (R.NE.FBLANK) THEN
                           RMAX = MAX (R, RMAX)
                           RMIN = MIN (R, RMIN)
                           END IF
 230                    CONTINUE
C                                       real, backward
                  ELSE IF (IXL.EQ.4) THEN
                     J = INPOS + NX
                     DO 240 I = 1,NX
                        R = RBUF2(J-I)
                        RBUF1(OUTPOS+I-1) = R
                        IF (R.NE.FBLANK) THEN
                           RMAX = MAX (R, RMAX)
                           RMIN = MIN (R, RMIN)
                           END IF
 240                    CONTINUE
                     END IF
 245              CONTINUE
 250           CONTINUE
C                                       now do writes
C                                       prepare write parms
C                                       LOOP: over rows
            DO 270 I2 = 1,NY
               INPOS = 1 + (I2 - 1) * LBYTES/2
               IB = 3 - IB
               IF (IDX.GE.2) THEN
                  CALL ZWAIT (OUTLUN, OUTIND, IB, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1120) IERR
                     GO TO 990
                     END IF
                  END IF
C                                       block to write
               JDEPTH(JAX(2)-2) = I2
               CALL COMOFF (CATBLK(KIDIM), NPST, JDEPTH, OBLKOF, IERR)
               IF (IERR.NE.0) GO TO 999
               IF (IDX.LT.10) IDX = IDX + 1
               I = IAX(2)
               IF (I.EQ.3) THEN
C                  ISC = (I3 + IOF(I)) * IS(I)
                  ISC = I3 - 1
               ELSE
                  ISC = (IDEPTH(I-2) + IOF(I)) * IS(I)
                  END IF
               ISC = IRC * ISC
               OBLKOF = OBLKOF + ISC
               OBLKOF = OBLKOF + 1
C                                       Write the row
               CALL ZMIO ('WRIT', OUTLUN, OUTIND, OBLKOF, LBYTES,
     *            RBUF1(INPOS), IB, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1220) IERR
                  GO TO 990
                  END IF
 270           CONTINUE
 290        CONTINUE
C                                       Wait for all writes to finish
         IB = 3 - IB
         CALL ZWAIT (OUTLUN, OUTIND, IB, IERR)
         IB = 3 - IB
         IF (IERR.EQ.0) CALL ZWAIT (OUTLUN, OUTIND, IB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1250) IERR
            GO TO 990
            END IF
         END IF
      IF (DONMAX.EQ.0) DONMAX = 1
      GO TO 999
C                                       Special case: short row move
C                                       No subimaging!
 500  IL = NBPS / (NX * 2)
      NV2 = NBUF2 / 2
      NV1 = NBUF1 / 2
      NVPS = NBPS / 2
      NS1 = NV1 / NVPS
      NS2 = NV2 / NVPS
C                                       Nullify relevant outer loop
      IF (IAX(2).EQ.3) THEN
         NYO = I3B
         I3B = 1
      ELSE IF (IAX(2).EQ.4) THEN
         NYO = I4B
         I4B = 1
      ELSE IF (IAX(2).EQ.5) THEN
         NYO = I5B
         I5B = 1
      ELSE IF (IAX(2).EQ.6) THEN
         NYO = I6B
         I6B = 1
      ELSE IF (IAX(2).EQ.7) THEN
         NYO = I7B
         I7B = 1
         END IF
C                                       Input buffer big enough?
 510  IF (NS1.LE.(NS2-1)/IL+2) THEN
         NS2 = NS2 - 1
         GO TO 510
         END IF
      NYI = NPI(2)
C                                       Outer loops over most things
      DO 570 I7 = I7A,I7B
      DO 570 I6 = I6A,I6B
      DO 570 I5 = I5A,I5B
      DO 570 I4 = I4A,I4B
      DO 570 I3 = I3A,I3B
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         DO 520 I = 3,7
            J = JAX(I) - 2
            IF (J.GT.0) JDEPTH(J) = IDEPTH(I-2)
 520        CONTINUE
C                                       Loop over sectors output ax 2
         DO 560 IYO = 1,NYO,IL
            NRIPS = MIN (IL, NYO-IYO+1)
            OBZERO = (IYO-1) / IL + 1
C                                       Loop over input ax 2 by output
C                                       buffer's full size
            DO 550 IYI = 1,NYI,NS2
               NRIR = MIN (NS2, NYI-IYI+1)
               I0 = (IYI-1) / IL
               J = (IYI+NRIR-2) / IL
               INBYTS = (J - I0 + 1) * NBPS
               IBZERO = I0 + 1
C                                       Fill output buffer by reading
C                                       NRIR rows from each of NRIPS
C                                       planes
               DO 535 IYO1 = 1,NRIPS
                  IDEPTH(IAX(2)-2) = IYO + IYO1 - 1
                  CALL COMOFF (CATBLK(KIDIM), NPI, IDEPTH, IBLKOF, IERR)
                  IBLKOF = IBLKOF + IBZERO
                  CALL ZMIO ('READ', INLUN, ININD, IBLKOF, INBYTS,
     *               RBUF1, 1, IERR)
                  IF (IERR.EQ.0) CALL ZWAIT (INLUN, ININD, 1, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1115) IERR
                     GO TO 990
                     END IF
                  OUTPS0 = (IYO1 - 1) * NX + 1
                  DO 530 IYI1 = 1,NRIR
                     I = IYI + IYI1 - 1
                     INPOS = 1 + NX * MOD (I-1, IL) + NVPS *
     *                  ((I-1)/IL - I0)
                     OUTPOS = OUTPS0 + (IYI1-1) * NVPS
                     CALL RCOPY (NX, RBUF1(INPOS), RBUF2(OUTPOS))
 530                 CONTINUE
 535              CONTINUE
C                                       Write out completed sectors
               DO 540 IYI1 = 1,NRIR
                  OUTPOS = 1 + (IYI1 - 1) * NBPS / 2
                  JDEPTH(JAX(2)-2) = IYI + IYI1 - 1
                  CALL COMOFF (CATBLK(KIDIM), NPST, JDEPTH, OBLKOF,
     *               IERR)
                  OBLKOF = OBLKOF + OBZERO
                  CALL ZMIO ('WRIT', OUTLUN, OUTIND, OBLKOF, NBPS,
     *               RBUF2(OUTPOS), 1, IERR)
                  IF (IERR.EQ.0) CALL ZWAIT (OUTLUN, OUTIND, 1, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1120) IERR
                     GO TO 990
                     END IF
 540              CONTINUE
 550           CONTINUE
 560        CONTINUE
 570     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('TRAROW: IO ERROR ON INIT READ',3I7)
 1110 FORMAT ('TRAROW: IO ERROR MINI3 WRITE',I7)
 1115 FORMAT ('TRAROW: IO ERROR ON READ',I7)
 1120 FORMAT ('TRAROW: IO ERROR ON WRITE',I7)
 1220 FORMAT ('TRAROW: IO ERROR ON INIT WRITE',I7)
 1250 FORMAT ('TRAROW: IO ERROR ON LAST WRITE',I7)
      END
      SUBROUTINE TRAXPO (IAX, NBUF1, NBUF2, RBUF1, RBUF2, IERR)
C-----------------------------------------------------------------------
C   TRAXPO performs the axis 1 with axis 2 transposition either in
C   core or with a two-pass disk-based method.  At the same time it
C   can swap axes 3 through 7 with themselves.  It does not perform
C   subarray selection nor find the output max/min.  These, if
C   needed, must be done first via a pass through TRAROW.
C   Inputs:
C      IAX     I(7)   IAX(output axis #) = input axis number
C      NBUF1   I      Size of 1st buffer in AIPS bytes
C      NBUF2   I      Size of 1st buffer in AIPS bytes
C   Output:
C      RBUF1   R(*)   I/O buffer (larger)
C      RBUF2   R(*)   I/O buffer (smaller)
C      IERR    I      IO error codes: >0 => quit
C-----------------------------------------------------------------------
      INTEGER   IAX(7), NBUF1, NBUF2, IERR
      REAL      RBUF1(*), RBUF2(*)
C
      INTEGER   JAX(7), IS(7), IOF(7), NPST(7), NBYPV, NVPS, NV1, NV2,
     *   NX, NY, AXI1, AXI2, NRI, NRO, IWIN(4), JWIN(4), IDEPTH(5),
     *   IBLKOF, OBLKOF, AXO1, AXO2, INPOS, NVGR, NGR, LUNSCR, INDSCR,
     *   ILX1, ILX2, ILY1, ILY2, NRM, NB, IOFF, JOFF, KOFF, NWM, NGPS,
     *   LSECT, NSECT, NXGR, NYGR, I, J, K, I2, I3, I4, I5, I6, I7,
     *   I3B, I4B, I5B, I6B, I7B, OUTPOS, BLKLX, JDEPTH(5), I1, I1B,
     *   IER, NBYTES, ISIZE, NSPGG, NSOFF, IGR, IGR0
      CHARACTER PHNAME*48
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'TRANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      INDSCR = 0
C                                       Get reverse of IAX
      DO 10 I = 1,7
         JAX(IAX(I)) = I
         NPST(I) = NPI(IAX(I))
         IF (RSGN(I).GT.0) THEN
            IS(I) = 1
            IOF(I) = 0
         ELSE
            IS(I) = -1
            IOF(I) = -1 - NPI(I)
            END IF
 10      CONTINUE
C                                       Outer DO loop limits, counters
      I3B = MAX (1, NPI(3))
      I4B = MAX (1, NPI(4))
      I5B = MAX (1, NPI(5))
      I6B = MAX (1, NPI(6))
      I7B = MAX (1, NPI(7))
C                                       Buffer sizes etc
      NBYPV = 2
      NVPS = NBPS / NBYPV
      NV1 = NBUF1 / NBYPV
      NV2 = NBUF2 / NBYPV
      NX = NPI(1)
      NY = NPI(2)
      BLKLX = (NY*NBYPV - 1) / NBPS + 1
      IF (NX*NBYPV.LE.NBPS/2) THEN
         ILX1 = NBPS / (NX * NBYPV)
         AXI1 = NX
         AXI2 = NVPS
      ELSE
         ILX1 = 1
         AXI1 = 0
         AXI2 = (1 + (NX-1)/NVPS) * NVPS
         END IF
      ILX2 = ((NV2 / 2) / AXI2) * 2
      NRI = ILX1 * ILX2
C                                       In core transpose!
      IF (NY.GT.NRI) GO TO 100
         IWIN(1) = 1
         IWIN(2) = 1
         WRITE (MSGTXT,1020)
         CALL MSGWRT (2)
         DO 90 I7 = 1,I7B
         DO 90 I6 = 1,I6B
         DO 90 I5 = 1,I5B
         DO 90 I4 = 1,I4B
         DO 90 I3 = 1,I3B
            IWIN(3) = NX
            IWIN(4) = NY
            IDEPTH(1) = I3
            IDEPTH(2) = I4
            IDEPTH(3) = I5
            IDEPTH(4) = I6
            IDEPTH(5) = I7
            CALL COMOFF (CATBLK(KIDIM), NPI, IDEPTH, IBLKOF, IERR)
            IF (IERR.NE.0) GO TO 999
            IBLKOF = IBLKOF + 1
            CALL MINIT ('READ', INLUN, ININD, NX, NY, IWIN, RBUF2,
     *         NBUF2, IBLKOF, IERR)
            IF (IERR.EQ.0) GO TO 24
               WRITE (MSGTXT,1024) IERR
               GO TO 990
C                                       Write bias in file
 24         DO 25 I = 3,7
               JDEPTH(JAX(I)-2) = (IDEPTH(I-2) + IOF(I))*IS(I)
 25            CONTINUE
            CALL COMOFF (CATBLK(KIDIM), NPST, JDEPTH, OBLKOF, IERR)
            IF (IERR.NE.0) GO TO 999
            OBLKOF = OBLKOF + 1
C                                       Read full plane
            DO 30 I = 1,NY
               CALL MDISK ('READ', INLUN, ININD, RBUF2, INPOS, IERR)
               IF (IERR.EQ.0) GO TO 30
                  WRITE (MSGTXT,1025) I,INPOS,IERR
                  GO TO 990
 30            CONTINUE
C                                       Write full plane
            IWIN(3) = NY
            IWIN(4) = NX
            CALL MINIT ('WRIT', OUTLUN, OUTIND, NY, NX, IWIN, RBUF1,
     *         NBUF1, OBLKOF, IERR)
            IF (IERR.EQ.0) GO TO 34
               WRITE (MSGTXT,1034) IERR
               GO TO 990
 34         DO 80 I = 1,NX
               CALL MDISK ('WRIT', OUTLUN, OUTIND, RBUF1, OUTPOS, IERR)
               IF (IERR.EQ.0) GO TO 35
                  WRITE (MSGTXT,1030) IERR
                  GO TO 990
 35            IF (ILX1.GT.1) GO TO 55
                  IOFF = I - AXI2
                  DO 50 J = 1,NY
                     IOFF = IOFF + AXI2
                     RBUF1(OUTPOS+J-1) = RBUF2(IOFF)
 50                  CONTINUE
                  GO TO 80
 55            CONTINUE
                  J = 0
                  DO 70 I2 = 1,ILX2
                     IOFF = I - AXI1 + (I2-1) * AXI2
                     DO 70 I1 = 1,ILX1
                        IOFF = IOFF + AXI1
                        RBUF1(OUTPOS+J) = RBUF2(IOFF)
                        J = J + 1
                        IF (J.EQ.NY) GO TO 80
 70                  CONTINUE
 80            CONTINUE
            CALL MDISK ('FINI', OUTLUN, OUTIND, RBUF1, INPOS, IERR)
            IF (IERR.EQ.0) GO TO 90
               WRITE (MSGTXT,1030) IERR
               GO TO 990
 90         CONTINUE
         GO TO 999
C                                       HARD CASE!!!
C                                       need a scratch file, 2 passes
 100  WRITE (MSGTXT,1100)
      CALL MSGWRT (2)
      IF (NY*NBYPV.GT.NBPS/2) GO TO 105
         ILY1 = NBPS / (NY * NBYPV)
         AXO1 = NY
         AXO2 = NVPS
         GO TO 110
 105  CONTINUE
         ILY1 = 1
         AXO1 = 0
         AXO2 = (1 + (NY-1)/NVPS) * NVPS
 110  ILY2 = ((NV2 / 2) / AXO2) * 2
      NRO = ILY1 * ILY2
      NRI = ILX1 * ILX2
C                                       Compute size of blocks hold
C                                       NRI points of NRO output rows
      NVGR = (1 + (NRI*NRO - 1)/NVPS) * NVPS
      IF (NVGR.LE.NV1) GO TO 115
         ILX2 = ILX2 / 2
         ILY2 = ILY2 / 2
         GO TO 110
 115  NGR = (1 + (NY-1)/NRI) * (1 + (NX-1)/NRO)
      NGPS = NVPS / (NRI * NRO)
      IF (NGPS.GT.1) THEN
         NVGR = NRI * NRO
         NSPGG = 0
         ISIZE = (NGR / NGPS) * NVPS * NBYPV
      ELSE
         NSPGG = NVGR / NVPS
         ISIZE = NGR * NVGR * NBYPV
         END IF
C                                       Requires image < 4 Gbytes
      ISIZE = (ISIZE - 1) / 512 + 1
      CALL SCREAT (ISIZE, WORK, IERR)
      IF (IERR.NE.0) GO TO 999
      LUNSCR = OUTLUN + 1
      CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, PHNAME, IERR)
      CALL ZOPEN (LUNSCR, INDSCR, SCRVOL(NSCR), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) GO TO 999
      JWIN(1) = 1
      JWIN(2) = 1
      JWIN(3) = NVGR
      JWIN(4) = NGR
      IWIN(1) = 1
      DO 250 I7 = 1,I7B
      DO 250 I6 = 1,I6B
      DO 250 I5 = 1,I5B
      DO 250 I4 = 1,I4B
      DO 250 I3 = 1,I3B
C                                       Read file depth
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         CALL COMOFF (CATBLK(KIDIM), NPI, IDEPTH, IBLKOF, IERR)
         IF (IERR.NE.0) GO TO 995
         IBLKOF = IBLKOF + 1
C                                       Out file depth
         DO 120 I = 3,7
            JDEPTH(JAX(I)-2) = (IDEPTH(I-2)+IOF(I)) * IS(I)
 120        CONTINUE
         CALL COMOFF (CATBLK(KIDIM), NPST, JDEPTH, OBLKOF, IERR)
         IF (IERR.NE.0) GO TO 995
         OBLKOF = OBLKOF + 1
C                                       Init scratch for write
         CALL MINIT ('WRIT', LUNSCR, INDSCR, NVGR, NGR, JWIN, RBUF1,
     *      NBUF1, 1, IERR)
         IF (IERR.EQ.0) GO TO 125
            WRITE (MSGTXT,1120) IERR
            GO TO 990
C                                       Pass 1: build scratch of NRI
C                                       points for each of NRO rows
C                                       in each scratch "row"
 125     DO 190 I2 = 1,NY,NRI
            IWIN(2) = I2
            NRM = MIN (NY-I2+1, NRI)
            IWIN(4) = I2 + NRM - 1
            IWIN(3) = NX
            CALL MINIT ('READ', INLUN, ININD, NX, NY, IWIN, RBUF2,
     *         NBUF2, IBLKOF, IERR)
            IF (IERR.EQ.0) GO TO 130
               WRITE (MSGTXT,1125) IERR
               GO TO 990
C                                       read in the NRI rows
 130        DO 135 I = 1,NRM
               CALL MDISK ('READ', INLUN, ININD, RBUF2, INPOS, IERR)
               IF (IERR.EQ.0) GO TO 135
                  WRITE (MSGTXT,1025) IERR
                  GO TO 990
 135           CONTINUE
            NB = (NX*NRI-1) / (NRO*NRI) + 1
            IOFF = 0
            DO 185 I = 1,NB
               CALL MDISK ('WRIT', LUNSCR, INDSCR, RBUF1, OUTPOS, IERR)
               IF (IERR.EQ.0) GO TO 140
                  WRITE (MSGTXT,1135) IERR
                  GO TO 990
 140           JOFF = OUTPOS - NRI - 1
               DO 180 K = 1,NRO
                  IOFF = IOFF + 1
                  IF (IOFF.GT.NX) GO TO 190
                     KOFF = IOFF - AXI2
                     JOFF = JOFF + NRI
                     IF (ILX1.GT.1) GO TO 160
                        DO 155 J = 1,NRM
                           KOFF = KOFF + AXI2
                           RBUF1(JOFF+J) = RBUF2(KOFF)
 155                       CONTINUE
                        GO TO 180
 160                 CONTINUE
                        J = 0
                        DO 175 I1B = 1,ILX2
                           KOFF = IOFF + (I1B-1)*AXI2 - AXI1
                           DO 175 I1 = 1,ILX1
                              J = J + 1
                              KOFF = KOFF + AXI1
                              RBUF1(JOFF+J) = RBUF2(KOFF)
                              IF (J.EQ.NRM) GO TO 180
 175                       CONTINUE
 180              CONTINUE
 185           CONTINUE
 190        CONTINUE
         CALL MDISK ('FINI', LUNSCR, INDSCR, RBUF1, OUTPOS, IERR)
         IF (IERR.EQ.0) GO TO 200
            WRITE (MSGTXT,1135) IERR
            GO TO 990
C                                       Parms for reading scratch file
 200     NBYTES = MAX (NVGR, NVPS) * NBYPV
         NXGR = 1 + (NX-1)/NRO
         NYGR = 1 + (NY-1)/NRI
         IGR0 = 0
         LSECT = -1000
         DO 245 I2 = 1,NX,NRO
            IWIN(3) = NY
            IWIN(2) = I2
            NWM = MIN (NX-I2+1, NRO)
            IWIN(4) = I2 + NWM - 1
            CALL MINIT ('WRIT', OUTLUN, OUTIND, NY, NX, IWIN, RBUF2,
     *         NBUF2, OBLKOF, IERR)
            IF (IERR.EQ.0) GO TO 205
               WRITE (MSGTXT,1200) IERR
               GO TO 990
C                                       Read all groups having data
C                                       for rows I2 - I2+NRO-1
 205        IGR0 = IGR0 + 1
            IGR = IGR0 - NXGR
            NB = (NY * NRO - 1) / (NRO*NRI) + 1
            DO 230 I = 1,NB
               IGR = IGR + NXGR
               IF (NGPS.GT.1) THEN
                  NSECT = (IGR-1) / NGPS + 1
                  NSOFF = MOD (IGR-1, NGPS) * NVGR + 1
               ELSE
                  NSECT = (IGR - 1) * NSPGG + 1
                  NSOFF = 1
                  END IF
               IF (NSECT.NE.LSECT) THEN
                  LSECT = NSECT
                  CALL ZMIO ('READ', LUNSCR, INDSCR, NSECT, NBYTES,
     *               RBUF1, 1, IERR)
                  IF (IERR.EQ.0) CALL ZWAIT (LUNSCR, INDSCR, 1, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1205) IERR
                     GO TO 990
                     END IF
                  END IF
               IOFF = (I-1) * NRI + 1
               IF (IOFF.GT.NY) GO TO 235
               NRM = MIN (NRI, NY-IOFF+1)
               JOFF = NSOFF - NRI
               IF (ILY1.GT.1) GO TO 220
                  IOFF = IOFF - AXO2
                  DO 215 K = 1,NWM
                     JOFF = JOFF + NRI
                     IOFF = IOFF + AXO2
                     CALL RCOPY (NRM, RBUF1(JOFF), RBUF2(IOFF))
 215                 CONTINUE
                  GO TO 230
 220           CONTINUE
                  K = 0
                  DO 225 I1B = 1,ILY2
                     KOFF = IOFF + (I1B-1)*AXO2 - AXO1
                     DO 225 I1 = 1,ILY1
                        JOFF = JOFF + NRI
                        KOFF = KOFF + AXO1
                        CALL RCOPY (NRM, RBUF1(JOFF), RBUF2(KOFF))
                        K = K + 1
                        IF (K.EQ.NWM) GO TO 230
 225                    CONTINUE
 230           CONTINUE
C                                       Write it all out
 235        DO 240 I = 1,NWM
               CALL MDISK ('WRIT', OUTLUN, OUTIND, RBUF2, INPOS, IERR)
               IF (IERR.EQ.0) GO TO 240
                  WRITE (MSGTXT,1030) IERR
                  GO TO 990
 240           CONTINUE
            CALL MDISK ('FINI', OUTLUN, OUTIND, RBUF2, INPOS, IERR)
            IF (IERR.EQ.0) GO TO 245
               WRITE (MSGTXT,1030) IERR
               GO TO 990
 245        CONTINUE
 250     CONTINUE
      GO TO 995
C
 990  CALL MSGWRT (8)
C
 995  IF (INDSCR.GT.0) CALL ZCLOSE (LUNSCR, INDSCR, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Begin in-core x-y transpose')
 1024 FORMAT ('TRAXPO: INIT INPUT READ ERROR',I7)
 1034 FORMAT ('TRAXPO: INIT OUTPUT WRITE ERROR',I7)
 1025 FORMAT ('TRAXPO: INPUT FILE READ ERROR',3I7)
 1030 FORMAT ('TRAXPO: OUTPUT FILE WRITE ERROR',3I7)
 1100 FORMAT ('Begin disk-based x-y transpose')
 1120 FORMAT ('TRAXPO: SCRATCH INIT WRITE ERROR',I7)
 1125 FORMAT ('TRAXPO: INPUT INIT READ ERROR',I7)
 1135 FORMAT ('TRAXPO: SCRATCH FILE WRITE ERROR',I7)
 1200 FORMAT ('TRAXPO: OUTPUT INIT WRITE ERROR',I7)
 1205 FORMAT ('TRAXPO: SCRATCH FILE READ ERROR',I7)
      END
