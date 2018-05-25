LOCAL INCLUDE 'CHKFC.INC'
      INCLUDE 'INCS:PCLN.INC'
      HOLLERITH XNAMEO(3), XBXFIL(12)
      REAL      XDISKO, XSEQO, XCOORD(6), CELSIZ(2), XSIZE(2),
     *   XFIELD, XRASH(MAXAFL), XDECSH(MAXAFL), XBOXES, XCLBOX(4,50),
     *   DOBLNK, DONUMB
      COMMON /INPARM/ XNAMEO, XSEQO, XDISKO, XBXFIL, XCOORD, CELSIZ,
     *   XSIZE, XFIELD, XRASH, XDECSH, XBOXES, XCLBOX, DOBLNK, DONUMB
C
      DOUBLE PRECISION RA0, DEC0
      INTEGER   IMSIZE(2), SEQOU, DISKOU, SCRTCH(256)
      LOGICAL   RQUICK
      CHARACTER BOXFIL*48, NAMEOU*12, CLASOU*6
      COMMON /SETFCP/ RA0, DEC0, IMSIZE, SEQOU, DISKOU, RQUICK
      COMMON /SETFCC/ BOXFIL, NAMEOU, CLASOU
LOCAL END
      PROGRAM CHKFC
C-----------------------------------------------------------------------
C! read a BOXFILE to make images of the Clean boxes
C# Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 2001, 2005, 2009, 2012
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
C   CHKFC makes images from an input BOXFILE of Clean boxes
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME       XNAMEI/NAMEIN    Name of input UV data.
C      INSEQ        SEQ/SEQIN        Seq. of input UV data.
C      INDISK       DISKIN/IDISK     Disk number of input UV data.
C      BOXFILE      XBXFIL/BOXFIL    BOXFILE output file name.
C      CELLSIZE     CELSIZ           pixel size in image.
C      IMSIZE       XSIZE/IMSIZE     image size, also field size.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PCLN.INC'
      INCLUDE 'CHKFC.INC'
      INTEGER   IRET, NFIELD, NBOXES(MAXFLD), WIN(4,MXNBFL),
     *   FLDSIZ(2,MAXFLD)
      DOUBLE PRECISION RAF(MAXFLD), DECF(MAXFLD)
C-----------------------------------------------------------------------
C                                       init and read BOXFILE
      CALL CHKFIN (NFIELD, NBOXES, WIN, FLDSIZ, RAF, DECF, IRET)
C                                       make images
      IF (IRET.EQ.0) CALL CHKFDO (NFIELD, NBOXES, WIN, FLDSIZ, RAF,
     *   DECF, IRET)
C
      CALL DIETSK (IRET, RQUICK, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE CHKFIN (NFIELD, NBOXES, WIN, FLDSIZ, RAF, DECF, IRET)
C-----------------------------------------------------------------------
C   Input routine - reads BOXFILE returning parameters
C   Output:
C      NFIELD   I        Max field number
C      NBOXES   I(*)     Max box number in field
C      WIN      I(4,*)   Box pixel numbers
C      FLDSIZ   I(2,*)   Field sizes
C      RAF      D(*)     Center RA of field
C      DECF     D(*)     Center DEC of field
C-----------------------------------------------------------------------
      INTEGER   NFIELD, NBOXES(*), WIN(4,*), FLDSIZ(2,*), IRET
      DOUBLE PRECISION RAF(*), DECF(*)
C
      INTEGER   NPARMS, IERR, IROUND, I
      CHARACTER PNAM*6
      INCLUDE 'CHKFC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA PNAM /'CHKFC'/
C-----------------------------------------------------------------------
C                                       initialize
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Initialize HITAB
      CALL HIINIT (3)
C                                       get the input parameters
      NPARMS = 231 + 2 * MAXAFL
      CALL GTPARM (PNAM, NPARMS, RQUICK, XNAMEO, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         RQUICK = .FALSE.
         IF (IERR.EQ.1) THEN
            MSGTXT = 'CANNOT FIND INITIATOR IN GTPARM'
            CALL MSGWRT (1)
            GO TO 999
         ELSE
            MSGTXT= 'DISK PROBLEMS IN GTPARM'
            CALL MSGWRT (6)
            GO TO 999
            END IF
         END IF
      IRET = 0
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
C                                       set parameters
      IMSIZE(1) = IROUND (XSIZE(1))
      IMSIZE(2) = IROUND (XSIZE(2))
      CALL H2CHR (48, 1, XBXFIL, BOXFIL)
      CALL H2CHR (12, 1, XNAMEO, NAMEOU)
      SEQOU = XSEQO + 0.5
      DISKOU = XDISKO + 0.5
      IF ((CELSIZ(1).LE.0.0) .OR. (CELSIZ(2).LE.0.0)) THEN
         MSGTXT = 'MUST SET CELLSIZE'
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
C                                       center coord
      I = 1
      IF ((XCOORD(1).LT.0.0) .OR. (XCOORD(2).LT.0.0) .OR.
     *   (XCOORD(3).LT.0.0)) I = -1
      RA0 = ABS(XCOORD(1))*15.0D0 + ABS(XCOORD(2))/4.0D0 +
     *   ABS(XCOORD(3))/240.0D0
      RA0 = I * RA0
      I = 1
      IF ((XCOORD(4).LT.0.0) .OR. (XCOORD(5).LT.0.0) .OR.
     *   (XCOORD(6).LT.0.0)) I = -1
      DEC0 = ABS(XCOORD(4)) + ABS(XCOORD(5))/60.0D0 +
     *   ABS(XCOORD(6))/3.6D3
      DEC0 = DEC0 * I
C                                       more adverbs
      NFIELD = XFIELD + 0.5
      DO 10 I = 1,NFIELD
         RAF(I) = RA0 + XRASH(I) / COS(DG2RAD*DEC0) / 3.6D3
         DECF(I) = DEC0 + XDECSH(I) / 3.6D3
 10      CONTINUE
C                                       read boxfile for nboxes, coords
      CALL FIELDF (BOXFIL, RA0, DEC0, NFIELD, FLDSIZ, RAF, DECF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       read boxfile for Clean windows
      CALL CWINDF (NFIELD, IMSIZE, BOXFIL, FLDSIZ, XBOXES, XCLBOX, WIN,
     *   NBOXES, IRET)
C
 999  RETURN
      END
      SUBROUTINE CHKFDO (NFIELD, NBOXES, WIN, FLDSIZ, RAF, DECF, IRET)
C-----------------------------------------------------------------------
C   Makes images
C   Input:
C      NFIELD   I        Max field number
C      NBOXES   I(*)     Max box number in field
C      WIN      I(4,*)   Box pixel numbers
C      FLDSIZ   I(2,*)   Field sizes
C      RAF      D(*)     Center RA of field
C      DECF     D(*)     Center DEC of field
C   Output:
C      IRET     I        Error code
C-----------------------------------------------------------------------
      INTEGER   NFIELD, NBOXES(*), WIN(4,NFIELD,*), FLDSIZ(2,*), IRET
      DOUBLE PRECISION RAF(*), DECF(*)
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   NX, NY, IY, IFIELD, I, SLOT, LUN, FIND, IWIN(4), NBY,
     *   IBLKOF, POS, J, JO, HLUN, HBUF(256), DATE(3), TIME(3), NC, IXC,
     *   IYC, LF
      CHARACTER OBJ*8, OBS*8, MTYPE*2, CDATE*12, CTIME*8, HILINE*72,
     *   BUN*8, TEL*8, FLDC*4
      REAL      BUFF(MABFSS), X, S
      INCLUDE 'CHKFC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA OBJ, OBS, BUN, TEL /'TESTFACE','CHKFC','JY/BEAM','AIPS'/
      DATA LUN, IBLKOF, HLUN /16, 1, 17/
C-----------------------------------------------------------------------
C                                       General header parms
      CALL CATINI (CATBLK)
      CALL CHR2H (8, OBJ, 1, CATH(KHOBJ))
      CALL CHR2H (8, OBS, 1, CATH(KHOBS))
      CALL CHR2H (8, BUN, 1, CATH(KHBUN))
      CALL CHR2H (8, TEL, 1, CATH(KHTEL))
      CALL CHR2H (8, 'RA---SIN', 1, CATH(KHCTP))
      CALL CHR2H (8, 'DEC--SIN', 1, CATH(KHCTP+2))
      CATR(KRCIC) = -CELSIZ(1) / 3600.0
      CATR(KRCIC+1) = CELSIZ(2) / 3600.0
      CATBLK(KIDIM) = 2
      CATR(KREPO) = 2000.0
      IF (NAMEOU.EQ.' ') NAMEOU = 'CHKFC images'
      CALL CHR2H (12, NAMEOU, KHIMNO, CATH(KHIMN))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      NBY = 2 * MABFSS
      CATR(KRDMN) = 0.0
      CATR(KRDMX) = 2.0
      IF (DONUMB.GT.0.0) CATR(KRDMX) = 3.0
      S = FBLANK
      IF (DOBLNK.LE.0.0) S = 0.0
      CATR(KRBLK) = S
      LF = 0
      IF (DONUMB.GT.0) LF = 1
      IF (DONUMB.GT.1.5) THEN
         I = (MAX (IMSIZE(1), FLDSIZ(1,1)) * 2) - 1
         CALL POWER2 (I, NX)
         I = (MAX (IMSIZE(2), FLDSIZ(2,1)) * 2) - 1
         CALL POWER2 (I, NY)
         I = MIN (NX/21, NY/9)
         LF = MAX (1, MIN (12, I/2))
         END IF
C                                       loop over fields (images)
      DO 100 IFIELD = 1,NFIELD
         WRITE (FLDC,1005) IFIELD
         CALL CHTRIM (FLDC, 4, FLDC, NC)
         I = (MAX (IMSIZE(1), FLDSIZ(1,IFIELD)) * 2) - 1
         CALL POWER2 (I, NX)
         CATBLK(KINAX) = NX
         I = (MAX (IMSIZE(2), FLDSIZ(2,IFIELD)) * 2) - 1
         CALL POWER2 (I, NY)
         CATBLK(KINAX+1) = NY
         CATD(KDCRV) = RAF(IFIELD)
         CATD(KDCRV+1) = DECF(IFIELD)
         CATR(KRCRP) = NX / 2
         CATR(KRCRP+1) = NY / 2 + 1
         WRITE (CLASOU,1010) IFIELD
         CALL CHR2H (6, CLASOU, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQOU
C                                       Create new cataloged file.
         CALL MCREAT (DISKOU, SLOT, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CREAT', IFIELD
            GO TO 990
            END IF
         SEQOU = CATBLK(KIIMS)
C                                       Open new file.
         MTYPE = 'MA'
         CALL MAPOPN ('INIT', DISKOU, NAMEOU, CLASOU, SEQOU, MTYPE,
     *      NLUSER, LUN, FIND, SLOT, CATBLK, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN', IFIELD
            GO TO 990
            END IF
C                                       Window for output file
         IWIN(1) = 1
         IWIN(2) = 1
         IWIN(3) = NX
         IWIN(4) = NY
         CALL MINIT ('WRIT', LUN, FIND, NX, NY, IWIN, BUFF, NBY,
     *      IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INITIALIZ', IFIELD
            GO TO 990
            END IF
         DO 50 IY = 1,NY
C                                       "write" before data
            CALL MDISK ('WRIT', LUN, FIND, BUFF, POS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRIT', IFIELD
               GO TO 980
               END IF
            CALL RFILL (NX, S, BUFF(POS))
            DO 30 I = 1,NBOXES(IFIELD)
C                                       rectangles
               IF (WIN(1,IFIELD,I).GT.0) THEN
                  IF ((IY.GE.WIN(2,IFIELD,I)) .AND.
     *               (IY.LE.WIN(4,IFIELD,I))) THEN
                     JO = WIN(1,IFIELD,I)
                     J = WIN(3,IFIELD,I) - JO + 1
                     IYC = (WIN(2,IFIELD,I) + WIN(4,IFIELD,I)) / 2
                     IXC = (WIN(1,IFIELD,I) + WIN(3,IFIELD,I)) / 2
                  ELSE
                     J = 0
                     END IF
C                                       circles
               ELSE
                  J = ABS(IY-WIN(4,IFIELD,I))
                  IF (J.LE.WIN(2,IFIELD,I)) THEN
                     X = SQRT (REAL(WIN(2,IFIELD,I))**2 - J*J)
                     IXC = WIN(3,IFIELD,I)
                     IYC = WIN(4,IFIELD,I)
                     J = IXC + X
                     JO = 2 * IXC - J
                     J = J - JO + 1
                  ELSE
                     J = 0
                     END IF
                  END IF
               IF (J.GT.0) THEN
                  JO = JO  + POS - 1
                  CALL RFILL (J, 1.0, BUFF(JO))
                  BUFF(JO) = 2.0
                  BUFF(JO+J-1) = 2.0
                  IF ((WIN(1,IFIELD,I).GT.0) .AND.
     *               ((IY.EQ.WIN(2,IFIELD,I)) .OR.
     *               (IY.EQ.WIN(4,IFIELD,I))))
     *               CALL RFILL (J, 2.0, BUFF(JO))
                  IF (DONUMB.GT.0.0) THEN
                     IF ((ABS(IY-IYC)/LF.LE.3) .AND. (J.GE.7*NC*LF)
     *                  .AND. (JO-POS+1.LE.IXC-(7*NC*LF)/2)) THEN
                        JO = IXC - (7*NC*LF)/2 + POS - 1
                        J = (IY - IYC + 4*LF + 1) / LF
                        CALL CHKCHR (FLDC, NC, LF, J, BUFF(JO))
                     ELSE IF ((ABS(IY-IYC).LE.3) .AND. (J.GE.7*NC)
     *                  .AND. (JO-POS+1.LE.IXC-(7*NC)/2)) THEN
                        JO = IXC - (7*NC)/2 + POS - 1
                        J = (IY - IYC + 4 + 1)
                        CALL CHKCHR (FLDC, NC, 1, J, BUFF(JO))
                        END IF
                     END IF
                  END IF
 30            CONTINUE
 50         CONTINUE
C                                       write last of data
         CALL MDISK ('FINI', LUN, FIND, BUFF, POS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FINISH', IFIELD
            GO TO 980
            END IF
C                                       Fake up history
         CALL HICREA (HLUN, DISKOU, SLOT, CATBLK, HBUF, IRET)
C                                       Write time and date on new file
         IF (IRET.EQ.0) THEN
            CALL ZDATE (DATE)
            CALL ZTIME (TIME)
            CALL TIMDAT (TIME, DATE, CTIME, CDATE)
            WRITE (HILINE,1050) TSKNAM, RLSNAM, CDATE, CTIME
            CALL HIADD (HLUN, HILINE, HBUF, IRET)
            END IF
         IF (IRET.EQ.0) CALL HENCOO (TSKNAM, NAMEOU, CLASOU, SEQOU,
     *      DISKOU, HLUN, HBUF, IRET)
         IF (IRET.EQ.0) THEN
            WRITE (HILINE,1055) TSKNAM, IFIELD, BOXFIL(:47)
            CALL HIADD (HLUN, HILINE, HBUF, IRET)
            END IF
         CALL HICLOS (HLUN, .TRUE., HBUF, IRET)
         IRET = 0
C                                       Close image
         CALL MAPCLS ('INIT', DISKOU, SLOT, LUN, FIND, CATBLK, .TRUE.,
     *      BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOS', IFIELD
            GO TO 990
            END IF
 100     CONTINUE
      GO TO 999
C
 980  CALL ZCLOSE (LUN, FIND, I)
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I6,1X,A,'ING IMAGE',I4)
 1005 FORMAT (I4)
 1010 FORMAT ('IIM',I3.3)
 1050 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
 1055 FORMAT (A6,'FIELD=',I4,' BOXFILE=',A)
      END
      SUBROUTINE FIELDF (BOXFIL, RA0, DEC0, NFIELD, FLDSIZ, RAF, DECF,
     *   IERR)
C-----------------------------------------------------------------------
C   Fills field parameters from the user test file C lines
C   Inputs:
C      BOXFIL   C*48     User provided file name containing box defs
C      RA0      D        Center RA
C      DEC0     D        Center Dec - used with F cards
C   Output:
C      NFIELD   I        Number of fields defined for field parms
C      FLDSIZ   I(2,*)   Field size parameter
C      RAF      D(*)     RA of fields
C      DEC      D(*)     Dec of fields
C      IERR     I        Error return code: 0 => no error
C-----------------------------------------------------------------------
      CHARACTER BOXFIL*48
      INTEGER   NFIELD, FLDSIZ(2,*), IERR
      DOUBLE PRECISION RA0, DEC0, RAF(*), DECF(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LUN, I, J, FIND, KBP, I1, I2, JT, JTRIM
      CHARACTER LINE*132, SIGN*1
      DOUBLE PRECISION X, XRA, XDEC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                        Open clean box file
      IF (BOXFIL.NE.' ') THEN
         LUN = 11
         CALL ZTXOPN ('READ', LUN, FIND, BOXFIL, .FALSE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                        Enter parameters from file
         I2 = 0
         DO 50 I = 1,50000
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.EQ.2) GO TO 60
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN'
               CALL MSGWRT (6)
               GO TO 999
               END IF
            JT = JTRIM (LINE)
C                                       parse for field:
C                                       #, fldsiz, rash, decsh
            IF ((LINE(:1).EQ.'F') .OR. (LINE(:1).EQ.'f')) THEN
               KBP = 2
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 50
               IF (X.GE.0.0D0) THEN
                  I1 = X + 0.50D0
               ELSE
                  I1 = X - 0.50D0
                  END IF
               IF ((I1.GE.1) .AND. (I1.LE.MAXFLD)) THEN
                  NFIELD = MAX (NFIELD, I1)
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  FLDSIZ(1,I1) = X + 0.50D0
                  IF (X.LT.0.0D0) FLDSIZ(1,I1) = -1
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  FLDSIZ(2,I1) = X + 0.50D0
                  IF (X.LT.0.0D0) FLDSIZ(2,I1) = -1
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  RAF(I1) = RA0 + X / COS(DG2RAD*DEC0) / 3.6D3
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  DECF(I1) = DEC0 + X / 3.6D3
                  I2 = I2 + 1
                  END IF
C                                       parse for ra/dec field
C                                       # fldsiz ra (HH MM SS) Dec (+-
C                                       DD MM SS)
            ELSE IF ((LINE(:1).EQ.'C') .OR. (LINE(:1).EQ.'c')) THEN
               KBP = 2
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 50
               IF (X.GE.0.0D0) THEN
                  I1 = X + 0.50D0
               ELSE
                  I1 = X - 0.50D0
                  END IF
               IF ((I1.GE.1) .AND. (I1.LE.MAXFLD)) THEN
                  NFIELD = MAX (NFIELD, I1)
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  FLDSIZ(1,I1) = X + 0.50D0
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  FLDSIZ(2,I1) = X + 0.50D0
C                                       RA
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XRA = X
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XRA = XRA + X/60.0D0
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XRA = XRA + X/3600.0D0
                  XRA = XRA * 15.0D0
C                                       Sign
                  J = KBP
 40               IF (J.GT.132) GO TO 990
                  IF (LINE(J:J).EQ.' ') THEN
                     J = J + 1
                     GO TO 40
                     END IF
                  SIGN = LINE(J:J)
C                                       DEC
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XDEC = ABS(X)
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XDEC = XDEC + X/60.0D0
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XDEC = XDEC + X/3600.0D0
                  IF (SIGN.EQ.'-') XDEC = -XDEC
                  RAF(I1) = XRA
                  DECF(I1) = XDEC
                  I2 = I2 + 1
                  END IF
               END IF
 50         CONTINUE
 60      CALL ZTXCLS (LUN, FIND, I)
         END IF
      IERR = 0
C
      IF (I2.GT.0) THEN
         MSGTXT = 'FIELDF: Final field parameters and # read are:'
         CALL MSGWRT (2)
         DO 80 J = 1,NFIELD
            WRITE (MSGTXT,1070) 'Field', J, FLDSIZ(1,J), FLDSIZ(2,J),
     *         RAF(J), DECF(J)
            CALL MSGWRT (2)
 80         CONTINUE
         END IF
      GO TO 999
C
 990  WRITE (MSGTXT,1990) I
      CALL MSGWRT (6)
      IERR = 1

 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIELDF: ERROR',I4,1X,A,'ING BOXFILE')
 1070 FORMAT (A5,I3,2I5,2F11.5)
 1990 FORMAT ('FIELDF: PARSING ERROR ON LINE',I4)
      END
      SUBROUTINE CWINDF (NFIELD, IMSIZE, BOXFIL, FLDSIZ, XBOXES, XCLBOX,
     *   WIN, NBOXES, IERR)
C-----------------------------------------------------------------------
C   Fills the WIN array with clean box definitions taken from BOXFIL
C   Inputs:
C      BOXFIL   C*48        User provided file name containing box defs
C      NFIELD   I           Number of fields defined
C   Outputs:
C      WIN      I(4,*,*)    clean boxes - defaulted on in (4,FIELD,BOX)
C      NBOXES   I*(*)       Array containing number of boxes/field
C      IERR     I           Error return code:
C                              0 => no error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER BOXFIL*48
      INTEGER   NFIELD, FLDSIZ(2,*), WIN(4,NFIELD,*), NBOXES(*)
      INTEGER   IMSIZE(2), IERR
      REAL      XBOXES, XCLBOX(4,50)
C
      INTEGER   LUN, I, J, ISUBF(MXNBOX), IFIELD, FIND, IPARM(5), KBP,
     *   I1, I2, IDD, LIMIT, NX, NY, II, IROUND, JT, JTRIM
      CHARACTER LINE*132
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      LIMIT = MIN (MXNBOX, MXNBFL / MAX(1,NFIELD))
C                                        Open clean box file
      IF (BOXFIL.NE.' ') THEN
         LUN = 11
         CALL ZTXOPN ('READ', LUN, FIND, BOXFIL, .FALSE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                        Enter box parameters from file
         CALL FILL (MXNBOX, 0, ISUBF)
         IDD = 0
         DO 50 I = 1,100000
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.EQ.2) GO TO 60
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN'
               CALL MSGWRT (6)
               GO TO 999
               END IF
            JT = JTRIM (LINE)
C                                       check for comments
            IF (LINE.EQ.' ') GO TO 50
            IF (LINE(:1).NE.' ') THEN
               IF (LINE(:1).LT.'0') GO TO 50
               IF (LINE(:1).GT.'9') GO TO 50
               END IF
C                                       parse for 5 integers
C                                       field, blc, trc
            KBP = 1
            CALL FILL (5, 0, IPARM)
            DO 30 J = 1,5
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) THEN
                  IF (J.EQ.1) GO TO 50
                  WRITE (MSGTXT,1020) I, J
                  CALL MSGWRT (6)
                  IERR = 1
                  GO TO 999
               ELSE
                  IF (X.GE.0.0D0) THEN
                     IPARM(J) = X + 0.50D0
                  ELSE
                     IPARM(J) = X - 0.50D0
                     END IF
                  END IF
 30            CONTINUE
            IFIELD = IPARM(1)
            IF ((IFIELD.GE.1) .AND. (IFIELD.LE.NFIELD)) THEN
               IDD = IDD + 1
               ISUBF(IFIELD) = ISUBF(IFIELD) + 1
               II = (MAX (IMSIZE(1), FLDSIZ(1,IFIELD)) * 2) - 1
               CALL POWER2 (II, NX)
               II = (MAX (IMSIZE(2), FLDSIZ(2,IFIELD)) * 2) - 1
               CALL POWER2 (II, NY)
               IF (ISUBF(IFIELD).LE.LIMIT) THEN
                  J = ISUBF(IFIELD)
C                                       circular
                  IF (IPARM(2).LT.0) THEN
                     WIN(1,IFIELD,J) = -1
                     WIN(3,IFIELD,J) = MAX (1+IPARM(3), MIN (IPARM(4),
     *                  NX-IPARM(3)))
                     WIN(4,IFIELD,J) = MAX (1+IPARM(3), MIN (IPARM(5),
     *                  NY-IPARM(3)))
                     WIN(2,IFIELD,J) = IPARM(3)
C                                       rectangular
                  ELSE
                     WIN(1,IFIELD,J) = MAX (1, MIN (IPARM(2), NX))
                     WIN(3,IFIELD,J) = MAX (1, MIN (IPARM(4), NX))
                     IF (WIN(3,IFIELD,J).LT.WIN(1,IFIELD,J)) THEN
                        I1 = WIN(3,IFIELD,J)
                        WIN(3,IFIELD,J) = WIN(1,IFIELD,J)
                        WIN(1,IFIELD,J) = I1
                        END IF
                     WIN(2,IFIELD,J) = MAX (1, MIN (IPARM(3), NY))
                     WIN(4,IFIELD,J) = MAX (1, MIN (IPARM(5), NY))
                     IF (WIN(4,IFIELD,J).LT.WIN(2,IFIELD,J)) THEN
                        I1 = WIN(4,IFIELD,J)
                        WIN(4,IFIELD,J) = WIN(2,IFIELD,J)
                        WIN(2,IFIELD,J) = I1
                        END IF
                     END IF
                  END IF
               END IF
 50         CONTINUE
 60      CALL ZTXCLS (LUN, FIND, I)
         END IF
C                                       defaults
      DO 70 I = 1,NFIELD
         IF ((I.EQ.1) .AND. (ISUBF(I).LE.0)) THEN
            I1 = XBOXES + 0.5
            DO 65 J = 1,I1
               WIN(1,1,J) = IROUND (XCLBOX(1,J))
               WIN(2,1,J) = IROUND (XCLBOX(2,J))
               WIN(3,1,J) = IROUND (XCLBOX(3,J))
               WIN(4,1,J) = IROUND (XCLBOX(4,J))
 65            CONTINUE
            ISUBF(1) = I1
            END IF
         IF (ISUBF(I).LE.0) THEN
            II = (MAX (IMSIZE(1), FLDSIZ(1,I)) * 2) - 1
            CALL POWER2 (II, NX)
            II = (MAX (IMSIZE(2), FLDSIZ(2,I)) * 2) - 1
            CALL POWER2 (II, NY)
            ISUBF(I) = 1
            WIN(1,I,1) = (NX - FLDSIZ(1,I)) / 2 + 1
            WIN(2,I,1) = (NY+1 - FLDSIZ(2,I)) / 2 + 1
            WIN(3,I,1) = WIN(1,I,1) + FLDSIZ(1,I) - 1
            WIN(4,I,1) = WIN(2,I,1) + FLDSIZ(2,I) - 1
            END IF
         IF (ISUBF(I).GT.0) NBOXES(I) = MIN (LIMIT, ISUBF(I))
         IF (ISUBF(I).GT.LIMIT) THEN
            WRITE (MSGTXT,1060) I, LIMIT, ISUBF(I)
            CALL MSGWRT (6)
            END IF
 70      CONTINUE
      IERR = 0
C
      IF (IDD.GT.0) THEN
         MSGTXT = 'WINDF: Number of clean boxes/field read as follows:'
         CALL MSGWRT(7)
         DO 80 J = 1,NFIELD,4
            I1 = J
            I2 = MIN (NFIELD, I1+3)
            WRITE (MSGTXT,1070) ('Field', I, NBOXES(I), I = I1,I2)
            CALL MSGWRT (3)
 80         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WINDF: ERROR',I4,1X,A,'ING THE CLEAN BOXES TEXT FILE')
 1020 FORMAT ('WINDF: PARSING ERROR ON LINE',I4,' FIELD',I2)
 1060 FORMAT ('WARNING: FIELD',I4,' USED FIRST',I5,' OF',I6,
     *   ' BOXES IN FILE')
 1070 FORMAT (4(A5,I3.2,':',I4,2X))
      END
      SUBROUTINE CHKCHR (STR, NC, LF, IY, BUFF)
C-----------------------------------------------------------------------
C   CHKCHR inserts characters into the data buffer 1 pixel row at a
C   time
C   Inputs:
C      STR    C*(*)   Character string
C      NC     I       Number of characters in STR
C      LF     I       Repetition factor
C      IY     I       Pixel number vertically within character string
C   Output:
C      BUFF   R(*)    Data buffer
C-----------------------------------------------------------------------
      CHARACTER STR*(*)
      INTEGER   NC, LF, IY
      REAL      BUFF(*)
C
      INTEGER   I, JT, LT, KT, BITS(15), SCRTCH(20), IDX, II, JJ,
     *   MASK, ZAND, TABLE(5,97), TAB2(5,17), TAB3(5,16), TAB4(5,16),
     *   TAB5(5,16), TAB6(5,16), TAB7(5,16), IT, IC, J
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE  (TABLE(1, 1), TAB2),  (TABLE(1,18), TAB3),
     *             (TABLE(1,34), TAB4),  (TABLE(1,50), TAB5),
     *             (TABLE(1,66), TAB6),  (TABLE(1,82), TAB7)
C                                        control chars all ?
C                                        blank !"#$%&'()*+,-./
      DATA TAB2/  32,  64,  69,  72,  48,
     *             0,   0,   0,   0,   0,
     *             0,   0, 121,   0,   0,
     *             0, 112,   0, 112,   0,
     *            20,  62,  20,  62,  20,
     *            18,  42, 127,  42,  36,
     *             2,  36,   8,  18,  32,
     *            54,  73,  85,  34,   5,
     *             0,   0, 112,   0,   0,
     *             0,  28,  34,  65,   0,
     *             0,  65,  34,  28,   0,
     *            20,   8,  62,   8,  20,
     *             8,   8,  62,   8,   8,
     *             0,   1,   6,   0,   0,
     *             0,   8,   8,   8,   0,
     *             0,   0,   1,   0,   0,
     *             2,   4,   8,  16,  32/
C                                        0123456789:;<=>?
      DATA TAB3/  62,  69,  73,  81,  62,
     *             0,  33, 127,   1,   0,
     *            35,  69,  73,  73,  49,
     *            66,  65,  73,  89, 102,
     *            12,  20,  36, 127,   4,
     *           114,  81,  81,  81,  78,
     *            30,  41,  73,  73,  70,
     *            64,  71,  72,  80,  96,
     *            54,  73,  73,  73,  54,
     *            49,  73,  73,  74,  60,
     *             0,   0,  18,   0,   0,
     *             0,   1,  22,   0,   0,
     *             8,  20,  34,  65,   0,
     *            20,  20,  20,  20,   0,
     *            65,  34,  20,   8,   0,
     *            32,  64,  69,  72,  48/
C
C                                       ABCDEFGHIJKLMNO
      DATA TAB4/  18,  37,  37,  37,  30,
     *            31,  36,  68,  36,  31,
     *           127,  73,  73,  73,  34,
     *            62,  65,  65,  65,  34,
     *            65, 127,  65,  65,  62,
     *           127,  73,  73,  73,  65,
     *           127,  72,  72,  64,  64,
     *            62,  65,  65,  69,  39,
     *           127,   8,   8,   8, 127,
     *             0,  65, 127,  65,   0,
     *             2,   1,   1,   1, 126,
     *           127,   8,  20,  34,  65,
     *           127,   1,   1,   1,   1,
     *           127,  32,  24,  32, 127,
     *           127,  16,   8,   4, 127,
     *            62,  65,  65,  65,  62/
C                                        PQRSTUVWXYZ[\]^_
      DATA TAB5/ 127,  72,  72,  72,  48,
     *            62,  65,  69,  66,  61,
     *           127,  72,  76,  74,  49,
     *            50,  73,  73,  73,  38,
     *            64,  64, 127,  64,  64,
     *           126,   1,   1,   1, 126,
     *           112,  12,   3,  12, 112,
     *           126,   1,  14,   1, 126,
     *            99,  20,   8,  20,  99,
     *            96,  16,  15,  16,  96,
     *            67,  69,  73,  81,  97,
     *             0,   0, 127,  65,   0,
     *            32,  16,   8,   4,   2,
     *             0,  65, 127,   0,   0,
     *            16,  32,  64,  32,  16,
     *             1,   1,   1,   1,   1/
C                                        `abcdefghijklmno
      DATA TAB6/   0,  64,  32,  16,  0,
     *            31,  36,  68,  36,  31,
     *           127,  73,  73,  73,  34,
     *            62,  65,  65,  65,  34,
     *            65, 127,  65,  65,  62,
     *           127,  73,  73,  73,  65,
     *           127,  72,  72,  64,  64,
     *            62,  65,  65,  69,  39,
     *           127,   8,   8,   8, 127,
     *            65,  65, 127,  65,  65,
     *             2,   1,   1,   1, 126,
     *           127,   8,  20,  34,  65,
     *           127,   1,   1,   1,   1,
     *           127,  32,  24,  32, 127,
     *           127,  16,   8,   4, 127,
     *            62,  65,  65,  65,  62/
C                                        pqrstuvwxyz{ }~?
      DATA TAB7/ 127,  72,  72,  72,  48,
     *            62,  65,  69,  66,  61,
     *           127,  72,  76,  74,  49,
     *            50,  73,  73,  73,  38,
     *            64,  64, 127,  64,  64,
     *           126,   1,   1,   1, 126,
     *           112,  12,   3,  12, 112,
     *           126,   1,  14,   1, 126,
     *            99,  20,   8,  20,  99,
     *            96,  16,  15,  16,  96,
     *            67,  69,  73,  81,  97,
     *             0,   8,  54,  65,   0,
     *             0,   0, 127,   0,   0,
     *             0,  65,  54,   8,   0,
     *             4,   8,   4,   2,   4,
     *            32,  64,  69,  72,  48/
C-----------------------------------------------------------------------
      IDX = 1
      DO 20 I = 1,NC
C                                       get standard ASCII char
C                                       in highly machine independent
         JT = NBITWD / 8
         CALL ZCLC8 (1, STR(I:I), JT, LT)
         CALL ZI32IL (1, 1, LT, KT)
         IT = NBITWD - (JT-1)*8
         CALL ZGTBIT (IT, KT, BITS)
         CALL ZPTBIT (8, IC, BITS(IT-7))
C                                       all CTRL characters to 1
         IC = MAX (1, IC-30)
         CALL COPY (5, TABLE(1,IC), SCRTCH(IDX))
         IDX = IDX + 5
 20      CONTINUE
C                                       just the selected row
      MASK = 2 ** (IY-1)
      JJ = 1
      IDX = 1
C                                        if mask bit on in char value
C                                        then set to -1 else to 0
      DO 40 J = 1,NC
         IDX = IDX + LF
         DO 35 II = 1,5
            IC = ZAND (MASK, SCRTCH(JJ))
            DO 30 I = 1,LF
               IF ((IC.NE.0) .AND. (BUFF(IDX).NE.FBLANK)) BUFF(IDX) =
     *            3.0 * BUFF(IDX)
               IDX = IDX + 1
 30            CONTINUE
            JJ = JJ + 1
 35         CONTINUE
         IDX = IDX + LF
 40      CONTINUE
C
 999  RETURN
      END

