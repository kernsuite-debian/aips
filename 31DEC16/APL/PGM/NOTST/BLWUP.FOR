      PROGRAM BLWUP
C-----------------------------------------------------------------------
C! Task to blow up an image.
C# Map-util Spectral Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2009
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
C   BLWUP will blow up a subarray of a cataloged file to a new file and
C   insert a catalog entry for the new file.
C   INPUTS:  (from AIPS)
C            INNAME     R(3)   the entry name for the source file.  If
C                       blank the first match consistent with the other
C                       parameters is used.
C            INCLASS    R(2)   the class of the source file.  If blank
C                       the first match consistent with the other
C                       parameters is used.
C            INSEQ      R   the sequence number of the source file.  If
C                       zero the first match consistent with the other
C                       parameters is used.
C            INDISK     R   the disk volume number of the source file.
C                       If zero all disks are searched and the first
C                       match found is used.
C            OUTNAME    R(3)   the name of the new subimage file.  If
C                       blank the value in INNAME will be used.
C            OUTCLASS   R(2)   the class of the new subimage file.  If
C                       blank the value in INCLASS will be used.
C            OUTSEQ     R   the sequence number for the new subimage
C                       file.  If zero the first sequence number that
C                       will produce a unique file specification is
C                       used.
C            OUTDISK    R   the disk volume number for the new subimage
C                       file.  If zero the new file will be created
C                       on the same disk as the old file.
C            BLC        R(7)   the coordinate in the source file to
C                       become the bottom left hand coordinate (1,1)
C                       of the subimage.  BLC(1) is the X value and
C                       BLC(2) is the Y value.  The first coordinate
C                       IN the source map is (1,1). Any BLC(I) equal
C                       to zero defaults to 1.
C            TRC        R(7)   the coordinate in the source file to
C                       become the top right hand corner of the
C                       subimage.  The conventions used for BLC hold.
C            FACTOR     Blowup factor
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER PRGNAM*6, NAMIN*12, CLSIN*6, NAMOUT*12, CLSOUT*6,
     *   IDNAME*12, IDCLAS*6, HILINE*72, CBLANK*6, TYPIN*2, NOTTYP*2,
     *   LINE*72, MTYPE*2
      HOLLERITH XNAMIN(3), XCLSIN(2), XNMOUT(3), XCLOUT(2)
      REAL      BLC(7), TRC(7), DSKIN, DSKOUT, SEQIN, SEQOUT, XFACT,
     *   RSBUFF(MABFSS), RDBUFF(MABFSS)
      INTEGER   IBLKOF, IDIMS(7), IDEPTH(5), ISWIN(4), IDWIN(4), I, J,
     *   L, K, IDIND, IDLUN, IDPOS, IDSEQ, IDSLOT, IDVOL, IDX,
     *   IDY,IERR, IH1LUN, IH2LUN,INDX3, INDX3L, INDX3U, INDX4, INDX4L,
     *   INDX4U, INDX5, INDX5L,INDX5U, INDX6, INDX6L, INDX6U, INDX7,
     *   INDX7L, INDX7U, INDEST,INODIM, INPRMS, INX, INY, IRETCD, ISIND,
     *   ISLUN, ISPOS,ISSEQ, ISSLOT, ISVOL, ISX, ISY, IUSER,NFILES,
     *   IROUND, FACTOR, OFFSET, IFFSET, FACTX, FACTY, NBY, MAXSIZ,
     *   SCRTCH(512)
      LOGICAL  QUICK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XNMOUT, XCLOUT,
     *   SEQOUT, DSKOUT, BLC, TRC, XFACT
      DATA CBLANK /' '/
      DATA PRGNAM /'BLWUP '/, NOTTYP /'  '/
      DATA ISLUN, IDLUN, IH1LUN, IH2LUN /17, 18, 27, 28/
      DATA NFILES /2/
C-----------------------------------------------------------------------
C                                       Initialize the I/O parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Get input values from AIPS.
      INPRMS = 29
      CALL GTPARM (PRGNAM, INPRMS, QUICK, XNAMIN, SCRTCH, IERR)
      IRETCD = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         END IF
C
      IF (QUICK) CALL RELPOP (IRETCD, SCRTCH, IERR)
      IF (IRETCD.NE.0) GO TO 995
      IRETCD = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL H2CHR (12, 1, XNMOUT, NAMOUT)
      CALL H2CHR (6, 1, XCLOUT, CLSOUT)
C                                       Set initial values.
      ISVOL = IROUND (DSKIN)
      ISSEQ = IROUND (SEQIN)
      IDVOL = IROUND (DSKOUT)
      IDSEQ = IROUND (SEQOUT)
      FACTOR = IROUND (XFACT)
      FACTOR = MAX (1, FACTOR)
      FACTX  = FACTOR
      FACTY  = FACTOR
      IUSER = NLUSER
      IDNAME = NAMOUT
      IDCLAS = CLSOUT
      MAXSIZ = 8192
C                                       Open source file.
      MTYPE = 'MA'
      CALL MAPOPN ('READ', ISVOL, NAMIN, CLSIN, ISSEQ, MTYPE, IUSER,
     *   ISLUN, ISIND, ISSLOT, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 995
      NBY = 2 * MABFSS
C                                       Set default values BLC, TRC.
      INODIM = CATBLK(KIDIM)
      CALL WINDOW (INODIM, CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 960
C
      ISWIN(1) = IROUND (BLC(1))
      ISWIN(2) = IROUND (BLC(2))
      ISWIN(3) = IROUND (TRC(1))
      ISWIN(4) = IROUND (TRC(2))
      IDX = ISWIN(3) - ISWIN(1) + 1
      IDY = ISWIN(4) - ISWIN(2) + 1
C                                       #pixels output image
      INX = FACTX * IDX
      INY = FACTY * IDY
      IF ((INX.GT.MAXSIZ) .OR. (INY.GT.MAXSIZ)) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (4)
         GO TO 995
         END IF
C                                       #pixels on each axis
      ISX = CATBLK(KINAX)
      ISY = CATBLK(KINAX + 1)
C                                       Save source file dim values.
      CALL COPY (7, CATBLK(KINAX), IDIMS)
C                                       Build new file cat name.
      CALL MAKOUT (NAMIN, CLSIN, ISSEQ, CBLANK, IDNAME, IDCLAS, IDSEQ)
C                                       Set header values needed
C                                       by MCREAT.
      CALL BLWHDR (BLC, TRC, FACTX, FACTY)
      CALL CHR2H (12, IDNAME, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, IDCLAS, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = IDSEQ
C                                       Create new cataloged file.
      CALL MCREAT (IDVOL, IDSLOT, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 960
      IDSEQ = CATBLK(KIIMS)
C                                       Copy any header keywords
      CALL KEYCOP (ISVOL, ISSLOT, IDVOL, IDSLOT, IERR)
C                                       Allow failure
      IERR = 0
C                                       Open new file.
      MTYPE = 'MA'
      CALL MAPOPN ('INIT', IDVOL, IDNAME, IDCLAS, IDSEQ, MTYPE, IUSER,
     *   IDLUN, IDIND, IDSLOT, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 940
C                                       Window for destination file.
      IDWIN(1) = 1
      IDWIN(2) = 1
      IDWIN(3) = INX
      IDWIN(4) = INY
C                                       Init values for loop.
      INDX3L = IROUND (BLC(3))
      INDX3U = IROUND (TRC(3))
      INDX4L = IROUND (BLC(4))
      INDX4U = IROUND (TRC(4))
      INDX5L = IROUND (BLC(5))
      INDX5U = IROUND (TRC(5))
      INDX6L = IROUND (BLC(6))
      INDX6U = IROUND (TRC(6))
      INDX7L = IROUND (BLC(7))
      INDX7U = IROUND (TRC(7))
C                                       Loop for all possible planes.
      DO 200 INDX7 = INDX7L,INDX7U
      DO 200 INDX6 = INDX6L,INDX6U
      DO 200 INDX5 = INDX5L,INDX5U
      DO 200 INDX4 = INDX4L,INDX4U
      DO 200 INDX3 = INDX3L,INDX3U
C                                       Set corner selection.
         IDEPTH(1) = INDX3
         IDEPTH(2) = INDX4
         IDEPTH(3) = INDX5
         IDEPTH(4) = INDX6
         IDEPTH(5) = INDX7
C                                       Block offset for source file.
         CALL COMOFF (CATBLK(KIDIM), IDIMS, IDEPTH, IBLKOF, IERR)
         IF (IERR.NE.0) GO TO 940
         IBLKOF = IBLKOF + 1
C                                       Initialize for double buffering
         CALL MINIT ('READ', ISLUN, ISIND, ISX, ISY, ISWIN, RSBUFF, NBY,
     *      IBLKOF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            CALL MSGWRT (7)
            GO TO 940
            END IF
C                                       Find block offset for subimage.
         IDEPTH(1) = INDX3 - INDX3L + 1
         IDEPTH(2) = INDX4 - INDX4L + 1
         IDEPTH(3) = INDX5 - INDX5L + 1
         IDEPTH(4) = INDX6 - INDX6L + 1
         IDEPTH(5) = INDX7 - INDX7L + 1
         CALL COMOFF (INODIM, CATBLK(KINAX), IDEPTH, IBLKOF, IERR)
         IF (IERR.NE.0) GO TO 940
         IBLKOF = IBLKOF + 1
C
         CALL MINIT ('WRIT', IDLUN, IDIND, INX, INY, IDWIN, RDBUFF, NBY,
     *      IBLKOF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1110) IERR
            CALL MSGWRT (7)
            GO TO 940
            END IF
C                                       The first write sets up indicies
         CALL MDISK ('WRIT', IDLUN, IDIND, RDBUFF, IDPOS, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1120) IERR
            CALL MSGWRT (7)
            GO TO 940
            END IF
C                                       Read from source, write to
C                                       subimage file.
         DO 140 I = 1,IDY
C                                       read row I
            CALL MDISK ('READ', ISLUN, ISIND, RSBUFF, ISPOS, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1115) IERR
               CALL MSGWRT (7)
               GO TO 940
               END IF
C                                       fill output buffer
            DO 120 J = 1, FACTY
C                                       loop over one line
               DO 110 L = 1, IDX
C                                       input offset
                  IFFSET = ISPOS + L - 1
C                                       output offset
                  OFFSET = IDPOS + (L - 1) * FACTX
                  DO 100 K = 1, FACTX
                     RDBUFF(OFFSET+K-1) = RSBUFF(IFFSET)
 100                 CONTINUE
 110              CONTINUE
C                                       Test if last buffer to write
               IF (I.NE.IDY.OR.J.NE.FACTY) THEN
                  CALL MDISK ('WRIT', IDLUN, IDIND, RDBUFF, IDPOS,IERR)
                  END IF
 120           CONTINUE
 140        CONTINUE
C                                       Write last buffer.
         CALL MDISK ('FINI', IDLUN, IDIND, RDBUFF, IDPOS, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1150) IERR
            CALL MSGWRT (7)
            GO TO 940
            END IF
 200     CONTINUE
C
      CALL HIINIT (NFILES)
C                                       Create and copy history file.
      CALL HISCOP (IH1LUN, IH2LUN, ISVOL, IDVOL, ISSLOT, IDSLOT,
     *   CATBLK, SCRTCH(257), SCRTCH, IERR)
      IF (IERR.GT.3) GO TO 300
      IF (IERR.EQ.3) GO TO 220
C                                       Add BLWUP history.
      CALL HENCO1 (PRGNAM, NAMIN, CLSIN, ISSEQ, ISVOL, IH2LUN,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 220
      TYPIN = 'MA'
      WRITE (LINE,1210) TYPIN, IUSER
      HILINE = LINE
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 220
      CALL HENCOO (PRGNAM, IDNAME, IDCLAS, IDSEQ, IDVOL, IH2LUN,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 220
      WRITE (LINE,1211) ISWIN(1), ISWIN(2), INDX3L, INDX4L, INDX5L,
     *   INDX6L, INDX7L
      HILINE = LINE
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 220
      WRITE (LINE,1212) ISWIN(3), ISWIN(4), INDX3U, INDX4U, INDX5U,
     *   INDX6U, INDX7U
      HILINE = LINE
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 220
      WRITE (LINE,1213) FACTX, FACTY
      HILINE = LINE
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 220
 220  CALL HICLOS (IH2LUN, .TRUE., SCRTCH, IERR)
C                                       Copy CC files and others
 300  CALL ALLTAB (0, NOTTYP, IH1LUN, IH2LUN, ISVOL, IDVOL, ISSLOT,
     *   IDSLOT, CATBLK, SCRTCH(257), SCRTCH, IERR)
C                                       Successful finish
 310  CALL MAPCLS ('READ', ISVOL, ISSLOT, ISLUN, ISIND, CATBLK, .FALSE.,
     *   SCRTCH, IERR)
      CALL MAPCLS ('INIT', IDVOL, IDSLOT, IDLUN, IDIND, CATBLK, .TRUE.,
     *   SCRTCH, IERR)
      IRETCD = 0
      GO TO 995
C-----------------------------------------------------------------------
C                                       Error. Delete catalog entry.
 940  CALL MAPCLS ('INIT', IDVOL, IDSLOT, IDLUN, IDIND, CATBLK, .FALSE.,
     *   SCRTCH, IERR)
      CALL MDESTR (IDVOL, IDSLOT, CATBLK, SCRTCH, INDEST, IERR)
C                                       Error. Close source file.
 960  CALL MAPCLS ('READ', ISVOL, ISSLOT, ISLUN, ISIND, CATBLK, .FALSE.,
     *   SCRTCH, IERR)
C                                       Release AIPS if wait state.
 995  CALL DIETSK (IRETCD, QUICK, SCRTCH)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. ERR=',I5)
 1010 FORMAT ('sorry, this factor makes the output map too big')
 1100 FORMAT ('MINI3 READ ERROR',I5)
 1110 FORMAT ('MINI3 WRITE ERROR',I5)
 1115 FORMAT ('DOUBLE BUFFERED READ ERROR',I5)
 1120 FORMAT ('DOUBLE BUFFERED WRITE ERROR',I5)
 1150 FORMAT ('MDISK FINI ERROR',I5)
 1210 FORMAT ('BLWUP INTYPE =''',A2,'''',6X,'USERID=',I5)
 1211 FORMAT ('BLWUP BLC    =',2(I5,','),4(I5,','),I5)
 1212 FORMAT ('BLWUP TRC    =',2(I5,','),4(I5,','),I5)
 1213 FORMAT ('BLWUP FACTOR X = ', I3,' Y = ', I3)
      END
      SUBROUTINE BLWHDR (BLC, TRC, FACTX, FACTY)
C-----------------------------------------------------------------------
C   BLWHDR corrects the header for subimaging: changes number of points
C   on the axes, the reference pixels, and the alternate axis (freq vs
C   velocity) reference pixel.  It corrects the first two axes for use
C   of pixel increments - namely the reference pixel and the axis incr.
C   Inputs:
C      BLC    R(7)        Bottom left corner to use
C      TRC    R(7)        Top right corner to use
C      FACTX  I           Blow up factor X
C      FACTY  I           Blow up factor Y
C   Common:
C      /MAPHDR/ CATBLK     map header (in/out)
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7)
C
      CHARACTER FCHARS(3)*4, CHTEMP*1
      HOLLERITH CATH(256)
      REAL      CATR(256), FACT(7)
      INTEGER   IPL, IPH, CATBLK(256), NAX, I, J, FACTX, FACTY
      DOUBLE PRECISION    CATD(128)
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Regular axis parameters
      NAX = CATBLK(KIDIM)
      CALL RFILL (7, 1.0, FACT)
      IF (FACTX.GT.0.001) FACT(1) = FLOAT (FACTX)
      IF (FACTY.GT.0.001) FACT(2) = FLOAT (FACTY)
      DO 10 I = 1, NAX
         IPL = BLC(I) + 0.01
         IPH = TRC(I) + 0.01
         CATBLK(KINAX+I-1) = (IPH - IPL + 1) * FACT(I)
         CATR(KRCRP+I-1) = (CATR(KRCRP+I-1) - IPL) * FACT(I) +
     *      (1.0 + FACT(I)) / 2.0
         CATR(KRCIC+I-1) = CATR(KRCIC+I-1) / FACT(I)
 10      CONTINUE
C                                       Alternate axis
      IF (CATBLK(KIALT).EQ.0) GO TO 999
         DO 25 I = 1,NAX
            IPL = KHCTP + (I-1) * 2
            CALL H2CHR (4, 1, CATH(IPL), CHTEMP)
            DO 20 J = 1,3
               IF (FCHARS(J).EQ.CHTEMP) GO TO 30
 20            CONTINUE
 25         CONTINUE
         GO TO 999
C                                       Found one
 30      CONTINUE
            IPL = BLC(I) + 0.01
            CATR(KRARP+I-1) = (CATR(KRARP+I-1) - IPL) * FACT(I) +
     *         (1.0 + FACT(I)) / 2.0
C
 999  RETURN
      END




