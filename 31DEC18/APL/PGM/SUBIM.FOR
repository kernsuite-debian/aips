LOCAL INCLUDE 'SUBIM.INC'
      INCLUDE 'INCS:PMAD.INC'
      HOLLERITH XNAMIN(3), XCLSIN(2), XNMOUT(3), XCLOUT(2), XOPCOD
      REAL      SEQIN, DSKIN, SEQOUT, DSKOUT, BLC(7), TRC(7), XINC,
     *   YINC, ZINC
      CHARACTER NAMIN*12, CLSIN*6, NAMOUT*12, CLSOUT*6, OPCODE*4,
     *   IMTYPE*2
      LOGICAL   DOMATH, DOAVE, DOMIN, DOMAX, DOSUM
      INTEGER   ISSEQ, ISVOL, ISSLOT, IDSEQ, IDVOL, IDSLOT, IXINC,
     *   IYINC, IZINC, ISBUFF(MABFSS), IDBUFF(MABFSS), ISLUN, ISIND,
     *   IDLUN, IDIND, IDIMS(7), SCRTCH(256), ISWIN(4), INDX3L, INDX3U,
     *   INDX4L, INDX4U, INDX5L, INDX5U, INDX6L, INDX6U, INDX7L, INDX7U,
     *   INODIM
      REAL      RSBUFF(MABFSS), RDBUFF(MABFSS)
C
      EQUIVALENCE (RSBUFF, ISBUFF, SCRTCH), (RDBUFF, IDBUFF)
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XNMOUT, XCLOUT,
     *   SEQOUT, DSKOUT, BLC, TRC, XINC, YINC, ZINC, XOPCOD
      COMMON /SUBIMP/ ISSEQ, ISVOL, ISSLOT, IDSEQ, IDVOL, IDSLOT, IXINC,
     *   IYINC, IZINC, DOMATH, DOAVE, DOMIN, DOMAX, ISLUN, ISIND, IDLUN,
     *   IDIND, IDIMS, INDX3L, INDX3U, INDX4L, INDX4U, INDX5L, INDX5U,
     *   INDX6L, INDX6U, INDX7L, INDX7U, ISWIN, INODIM, DOSUM
      COMMON /SUBIMB/ RSBUFF, RDBUFF
      COMMON /SUBIMC/ NAMIN, CLSIN, NAMOUT, CLSOUT, OPCODE, IMTYPE
LOCAL END
      PROGRAM SUBIM
C-----------------------------------------------------------------------
C! Task to copy subarray of image to another file
C# Map-util Spectral Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2009, 2013, 2015, 2017
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
C   SUBIM will copy a subarray of a cataloged file to a new file and
C   insert a catalog entry for the new file. Multi-dimensional rec-
C   tangles may be selected.  This routine runs as a detached task
C   initiated from AIPS.  It copies any tables found.
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
C            XINC       The X increment to use when selecting X pixels
C                       from the source file. Any TRC(I) equal to zero
C                       defaults to the maximum corresponding dimension
C                       in the source file header.
C            YINC       The Y increment to use when selecting lines
C                       from the source file.
C            ZINC       The Z increment (on plane 3)
C            OPCODE     '   ' take the nth value
C                       'AVE' Average values in XINC, YINC box
C                       'MIN' Take Minimum value in XINC, YINC box
C                       'MAX' Take Maximum value in XINC, YINC box
C                       'SUM' Sum values
C-----------------------------------------------------------------------
      INCLUDE 'SUBIM.INC'
      INTEGER   IRET, NX, NY, NWORDS, IERR
      CHARACTER PRGNAM*6
      LONGINT   PIMAGE, PWEIGH
      REAL      IMAGE(2), WEIGHT(2)
      INTEGER   COUNT(2)
      EQUIVALENCE (COUNT, WEIGHT)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'SUBIM'/
C-----------------------------------------------------------------------
C                                       init and create output file
      CALL SUBIMI (PRGNAM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       average over ZINC
      IF ((DOMATH) .AND. (IZINC.GT.1)) THEN
         NX = ((ISWIN(3) - ISWIN(1)) / IXINC) + 1
         NY = ((ISWIN(4) - ISWIN(2)) / IYINC) + 1
         NWORDS = (NX * NY - 1) / 1024 + 3
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, IMAGE, PIMAGE, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, WEIGHT,
     *      PWEIGH, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'FAILED TO GET NEEDED DYNAMIC MEMORY'
            CALL MSGWRT (8)
         ELSE
            CALL SUBIMF (NX, NY, IMAGE(1+PIMAGE), COUNT(1+PWEIGH), IRET)
            END IF
C                                       simpler subim
      ELSE
         CALL SUBIMD (IRET)
         END IF
C                                       history
      IF (IRET.EQ.0) CALL SUBIMH
C                                       Release AIPS if wait state.
 990  IF ((DOMATH) .AND. (IZINC.GT.1)) CALL ZMEMRY ('FRAL', TSKNAM,
     *   NWORDS, IMAGE, PIMAGE, IERR)
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE SUBIMI (PRGNAM, IRET)
C-----------------------------------------------------------------------
C   Init the task and crete the output image
C   Inputs:
C      PRGNAM   C*6   Task name
C   Outputs:
C      IRET     I     Error code: > 0 => quit
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*(*)
      INTEGER   IRET
C
      INCLUDE 'SUBIM.INC'
      INTEGER   INPRMS, IERR, IROUND, IUSER, I
      LOGICAL   CHANGE
      CHARACTER MTYPE*2, IDNAME*12, IDCLAS*6, CBLANK*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA CBLANK /' '/
C-----------------------------------------------------------------------
C                                       Initialize the I/O parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       Get input values from AIPS.
      INPRMS = 32
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XNAMIN, SCRTCH, IERR)
      IRET = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         END IF
C
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL H2CHR (12, 1, XNMOUT, NAMOUT)
      CALL H2CHR (6, 1, XCLOUT, CLSOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
C                                       Average, Minimum or Maximum
      DOAVE = OPCODE(:3).EQ.'AVE'
      DOMIN = OPCODE(:3).EQ.'MIN'
      DOMAX = OPCODE(:3).EQ.'MAX'
      DOSUM = OPCODE(:3).EQ.'SUM'
      DOMATH= DOMIN .OR. DOMAX .OR. DOAVE .OR. DOSUM
C                                       Set initial values.
      ISVOL = IROUND (DSKIN)
      ISSEQ = IROUND (SEQIN)
      IDVOL = IROUND (DSKOUT)
      IDSEQ = IROUND (SEQOUT)
      IXINC = IROUND (XINC)
      IYINC = IROUND (YINC)
      IZINC = IROUND (ZINC)
      IXINC = MAX (1, IXINC)
      IYINC = MAX (1, IYINC)
      IZINC = MAX (1, IZINC)
      XINC = IXINC
      YINC = IYINC
      ZINC = IZINC
      IUSER = NLUSER
      ISLUN = 17
      IDLUN = 18
C                                       Open source file.
      IMTYPE = '  '
      CALL MAPOPN ('READ', ISVOL, NAMIN, CLSIN, ISSEQ, IMTYPE, IUSER,
     *   ISLUN, ISIND, ISSLOT, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = ISVOL
      FCNO(NCFILE) = ISSLOT
      FRW(NCFILE) = 0
      IF ((IMTYPE.EQ.'UV') .OR. (CATBLK(KIPCN).GT.0)) THEN
         MSGTXT = 'DOES NOT WORK ON UV DATA SETS'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Save source file dim values.
      CALL COPY (7, CATBLK(KINAX), IDIMS)
C                                       Set default values BLC, TRC.
      INODIM = CATBLK(KIDIM)
      CALL WINDOW (INODIM, IDIMS, BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Build new file cat name.
      IDNAME = NAMOUT
      IDCLAS = CLSOUT
      CALL MAKOUT (NAMIN, CLSIN, ISSEQ, CBLANK, IDNAME, IDCLAS, IDSEQ)
      IF (IDNAME.EQ.'SCRATCH FILE') IDNAME = 'WAS SCRATCH'
C                                       Set header values needed
C                                       by MCREAT.
      CALL SUBHDR (BLC, TRC, XINC, YINC, ZINC)
      CALL CHR2H (12, IDNAME, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, IDCLAS, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = IDSEQ
      NAMOUT = IDNAME
      CLSOUT = IDCLAS
C                                       Create new cataloged file.
      CALL MCREAT (IDVOL, IDSLOT, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      IDSEQ = CATBLK(KIIMS)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = IDVOL
      FCNO(NCFILE) = IDSLOT
      FRW(NCFILE) = 2
C                                       Copy any header keywords
      CALL KEYCOP (ISVOL, ISSLOT, IDVOL, IDSLOT, IERR)
C                                       Allow failure
      IERR = 0
C                                       Open new file.
      MTYPE = 'MA'
      CALL MAPOPN ('INIT', IDVOL, IDNAME, IDCLAS, IDSEQ, MTYPE, IUSER,
     *   IDLUN, IDIND, IDSLOT, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      IRET = 0
      CHANGE = .FALSE.
      I = TRC(1) - BLC(1) + 1.1
      IF (I.NE.(I/IXINC)*IXINC) THEN
         CHANGE = .TRUE.
         I = (I / IXINC) * IXINC
         TRC(1) = BLC(1) + I - 1
         END IF
      I = TRC(2) - BLC(2) + 1.1
      IF (I.NE.(I/IYINC)*IYINC) THEN
         CHANGE = .TRUE.
         I = (I / IYINC) * IYINC
         TRC(2) = BLC(2) + I - 1
         END IF
      I = TRC(3) - BLC(3) + 1.1
      IF (I.NE.(I/IZINC)*IZINC) THEN
         CHANGE = .TRUE.
         I = (I / IZINC) * IZINC
         TRC(3) = BLC(3) + I - 1
         END IF
      IF (CHANGE) THEN
         MSGTXT = 'TRC ALTERED TO MATCH INCREMENTS'
         CALL MSGWRT (6)
         END IF
      ISWIN(1) = IROUND (BLC(1))
      ISWIN(2) = IROUND (BLC(2))
      ISWIN(3) = IROUND (TRC(1))
      ISWIN(4) = IROUND (TRC(2))
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
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. ERR=',I5)
      END
      SUBROUTINE SUBIMD (IRET)
C-----------------------------------------------------------------------
C   Does simpler SUBIM operation - either taking single voxels or
C   averaging only in X and Y.
C   Outputs:
C      IRET   I   > 0 => failure
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'SUBIM.INC'
C
      REAL      TEMP, RMAX, RMIN, SUMBUF(MABFSS)
      INTEGER   IBLKOF, IDEPTH(5), IDWIN(4), I, IDPOS, IDX, IDY, II,
     *   III, INDEX, IM, INDX3, INDX4, INDX5, INDX6, INDX7, ISPOS, ISX,
     *   ISY, NBY, NCOUNT(MAXIMG)
      LOGICAL   MAP, SAVE, WASBLK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (MAP, SAVE)
      DATA MAP /.TRUE./
C-----------------------------------------------------------------------
      WASBLK = .FALSE.
      NBY = 2 * MABFSS
C
      ISX = ISWIN(3) - ISWIN(1) + 1
      ISY = ISWIN(4) - ISWIN(2) + 1
      IDX = ((ISWIN(3) - ISWIN(1)) / IXINC) + 1
      IDY = ((ISWIN(4) - ISWIN(2)) / IYINC) + 1
C                                       Window for destination file.
      IDWIN(1) = 1
      IDWIN(2) = 1
      IDWIN(3) = IDX
      IDWIN(4) = IDY
C                                       Init values for loop.
      RMIN = MAX (1.0E20, ABS(CATR(KRDMX)), ABS(CATR(KRDMN)))
      RMAX = -RMIN
C                                       Loop for all possible planes.
      DO 200 INDX7 = INDX7L,INDX7U
      DO 200 INDX6 = INDX6L,INDX6U
      DO 200 INDX5 = INDX5L,INDX5U
      DO 200 INDX4 = INDX4L,INDX4U
      DO 200 INDX3 = INDX3L,INDX3U,IZINC
C                                       Set corner selection.
         IDEPTH(1) = INDX3
         IDEPTH(2) = INDX4
         IDEPTH(3) = INDX5
         IDEPTH(4) = INDX6
         IDEPTH(5) = INDX7
C                                       Block offset for source file.
         CALL COMOFF (INODIM, IDIMS, IDEPTH, IBLKOF, IRET)
         IF (IRET.NE.0) GO TO 999
         IBLKOF = IBLKOF + 1
C                                       Initialize for double buffering
         CALL MINIT ('READ', ISLUN, ISIND, IDIMS(1), IDIMS(2), ISWIN,
     *      RSBUFF, NBY, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            CALL MSGWRT (7)
            GO TO 999
            END IF
C                                       Find block offset for subimage.
         IDEPTH(1) = (INDX3 - INDX3L) / IZINC + 1
         IDEPTH(2) = INDX4 - INDX4L + 1
         IDEPTH(3) = INDX5 - INDX5L + 1
         IDEPTH(4) = INDX6 - INDX6L + 1
         IDEPTH(5) = INDX7 - INDX7L + 1
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IBLKOF = IBLKOF + 1
C
         CALL MINIT ('WRIT', IDLUN, IDIND, IDX, IDY, IDWIN, RDBUFF, NBY,
     *      IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1110) IRET
            CALL MSGWRT (7)
            GO TO 999
            END IF
C                                       Read from source, write to
C                                       subimage file.
         CALL FILL (IDX, 0, NCOUNT)
         DO 150 I = 1,ISY
C                                       read all rows
            CALL MDISK ('READ', ISLUN, ISIND, RSBUFF, ISPOS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1115) IRET
               CALL MSGWRT (7)
               GO TO 999
               END IF
C                                       If do math on region
C                                       Init index to write buffer.
            IM = 0
C                                       for all ranges on row
            DO 130 II = 1,ISX,IXINC
C                                       Increment tranfer index
               IM = IM + 1
C                                       For all pixels in range
               IF (DOMATH) THEN
                  DO 120 III = 1,IXINC
C                                       Calc index to range
                     INDEX = II + III - 2
C                                       If still in range
                     IF ((INDEX.GE.0) .AND. (INDEX.LT.ISX)) THEN
                        TEMP = RSBUFF(ISPOS+INDEX)
C                                       If not blanked
                        IF (TEMP.NE.FBLANK) THEN
C                                       If no counts yet
C                                       Just copy value
                           IF (NCOUNT(IM).EQ.0) THEN
                              SUMBUF(IM) = TEMP
C                                       Add to sum
                           ELSE IF ((DOAVE) .OR. (DOSUM)) THEN
                              SUMBUF(IM) = SUMBUF(IM) + TEMP
C                                       Return minimum
                           ELSE IF (DOMIN) THEN
                              SUMBUF(IM) = MIN (SUMBUF(IM),TEMP)
C                                       Return Maximum
                           ELSE IF (DOMAX) THEN
                              SUMBUF(IM) = MAX (SUMBUF(IM),TEMP)
                              END IF
C                                       increment unblanked pixel count
                           NCOUNT(IM) = NCOUNT(IM) + 1
                           END IF
                        END IF
 120                 CONTINUE
               ELSE IF (MOD(I-1,IYINC).EQ.0) THEN
                  SUMBUF(IM) = RSBUFF(ISPOS+II-1)
                  NCOUNT(IM) = 1
                  END IF
 130           CONTINUE
C                                       Do actual write
C                                       If last of range and math
            IF ((I/IYINC)*IYINC.EQ.I) THEN
C                                       "write" to set index
               CALL MDISK ('WRIT', IDLUN, IDIND, RDBUFF, IDPOS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) IRET
                  CALL MSGWRT (7)
                  GO TO 999
                  END IF
C                                       Init transfer index
               IM = 0
C                                       Scale, Get Min, Max
               DO 140 II = 1,ISX,IXINC
C                                       Increment transfer index
                  IM = IM + 1
C                                       If averaging
C                                       If unblanked data, scale
                  IF (NCOUNT(IM).GT.0) THEN
C                                       Transfer extream or Scale Sum
                     IF (DOAVE) THEN
                        TEMP = SUMBUF(IM) / NCOUNT(IM)
                     ELSE
                        TEMP = SUMBUF(IM)
                        END IF
C                                       Else blanked
                  ELSE
                     TEMP = FBLANK
                     END IF
C                                       Record extreama
                  IF (TEMP.NE.FBLANK) THEN
                     RMAX = MAX (RMAX, TEMP)
                     RMIN = MIN (RMIN, TEMP)
                  ELSE
                     WASBLK = .TRUE.
                     END IF
C                                       Transfer data
                  RDBUFF(IDPOS+IM-1) = TEMP
 140              CONTINUE
C                                       If averaging, init re-counts
               CALL FILL (IDX, 0, NCOUNT)
               END IF
C                                       End for all rows loop
 150        CONTINUE
C                                       Write last buffer.
         CALL MDISK ('FINI', IDLUN, IDIND, RDBUFF, IDPOS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1150) IRET
            CALL MSGWRT (7)
            GO TO 999
            END IF
 200     CONTINUE
C                                       Create new header.
C                                       Start with old header info.
      IF (RMAX.EQ.RMIN) THEN
         WRITE (MSGTXT,1200) RMAX
         CALL MSGWRT (6)
      ELSE IF (RMAX.LT.RMIN) THEN
         MSGTXT = 'NO VALID PIXELS FOUND'
         CALL MSGWRT (7)
         END IF
      CATR(KRDMN) = RMIN
      CATR(KRDMX) = RMAX
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('MINIT READ ERROR',I5)
 1110 FORMAT ('MINIT WRITE ERROR',I5)
 1115 FORMAT ('DOUBLE BUFFERED READ ERROR',I5)
 1120 FORMAT ('DOUBLE BUFFERED WRITE ERROR',I5)
 1150 FORMAT ('MDISK FINI ERROR',I5)
 1200 FORMAT ('Warning: subimage constant at',1PE13.5)
      END
      SUBROUTINE SUBIMF (NX, NY, IMAGE, COUNT, IRET)
C-----------------------------------------------------------------------
C   Does simpler SUBIM operation - either taking single voxels or
C   averaging only in X and Y.
C   Note that DOMATH is true and IZINC > 1 in this routine
C   Outputs:
C      IRET   I   > 0 => failure
C-----------------------------------------------------------------------
      INTEGER   NX, NY, COUNT(NX,NY), IRET
      REAL      IMAGE(NX,NY)
C
      INCLUDE 'SUBIM.INC'
C
      REAL      TEMP, RMAX, RMIN
      INTEGER   IBLKOF, IDEPTH(5), IDWIN(4), I, IDPOS, IDX, IDY, II,
     *   III, INDEX, IM, IN, INDX3, INDX4, INDX5, INDX6, INDX7, ISPOS,
     *   ISX, ISY, NBY, NPTS, INSX3
      LOGICAL   MAP, SAVE, WASBLK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (MAP, SAVE)
      DATA MAP /.TRUE./
C-----------------------------------------------------------------------
      WASBLK = .FALSE.
      NBY = 2 * MABFSS
C
      ISX = ISWIN(3) - ISWIN(1) + 1
      ISY = ISWIN(4) - ISWIN(2) + 1
      IDX = (ISWIN(3) - ISWIN(1)) / IXINC + 1
      IDY = (ISWIN(4) - ISWIN(2)) / IYINC + 1
C                                       Window for destination file.
      IDWIN(1) = 1
      IDWIN(2) = 1
      IDWIN(3) = NX
      IDWIN(4) = NY
      NPTS = NX * NY
C                                       Init values for loop.
      RMIN = MAX (1.0E20, ABS(CATR(KRDMX)), ABS(CATR(KRDMN)))
      RMAX = -RMIN
C                                       Loop for all possible planes.
      DO 200 INDX7 = INDX7L,INDX7U
      DO 200 INDX6 = INDX6L,INDX6U
      DO 200 INDX5 = INDX5L,INDX5U
      DO 200 INDX4 = INDX4L,INDX4U
      DO 200 INDX3 = INDX3L,INDX3U,IZINC
C                                       zero summing locations
         CALL RFILL (NPTS, 0.0, IMAGE)
         CALL FILL (NPTS, 0, COUNT)
C                                       Find block offset for subimage.
         IDEPTH(1) = (INDX3 - INDX3L) / IZINC + 1
         IDEPTH(2) = INDX4 - INDX4L + 1
         IDEPTH(3) = INDX5 - INDX5L + 1
         IDEPTH(4) = INDX6 - INDX6L + 1
         IDEPTH(5) = INDX7 - INDX7L + 1
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IBLKOF = IBLKOF + 1
C
         CALL MINIT ('WRIT', IDLUN, IDIND, NX, NY, IDWIN, RDBUFF, NBY,
     *      IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1110) IRET
            CALL MSGWRT (7)
            GO TO 999
            END IF
C                                       loop over input planes
C                                       for this output plane
         DO 150 INSX3 = INDX3,INDX3+IZINC-1
C                                       Set corner selection.
            IDEPTH(1) = INSX3
            IDEPTH(2) = INDX4
            IDEPTH(3) = INDX5
            IDEPTH(4) = INDX6
            IDEPTH(5) = INDX7
C                                       Block offset for source file.
            CALL COMOFF (INODIM, IDIMS, IDEPTH, IBLKOF, IRET)
            IF (IRET.NE.0) GO TO 999
            IBLKOF = IBLKOF + 1
C                                       Initialize for double buffering
            CALL MINIT ('READ', ISLUN, ISIND, IDIMS(1), IDIMS(2), ISWIN,
     *         RSBUFF, NBY, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) IRET
               CALL MSGWRT (7)
               GO TO 999
               END IF
C                                       Read from source, add to
C                                       subimage array
            DO 140 I = 1,ISY
C                                       read all rows
               CALL MDISK ('READ', ISLUN, ISIND, RSBUFF, ISPOS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1115) IRET
                  CALL MSGWRT (7)
                  GO TO 999
                  END IF
C                                       Init index to write buffer.
               IM = 0
               IN = (I - 1) / IYINC + 1
               DO 130 II = 1,ISX,IXINC
                  IM = IM + 1
C                                       For all pixels in range
                  DO 120 III = 1,IXINC
C                                       Calc index to range
                     INDEX = II + III - 2
C                                       If still in range
                     IF ((INDEX.GE.0) .AND. (INDEX.LT.ISX)) THEN
                        TEMP = RSBUFF(ISPOS+INDEX)
                        IF (TEMP.NE.FBLANK) THEN
C                                       If no counts yet
C                                       Just copy value
                           IF (COUNT(IM,IN).LE.0) THEN
                              IMAGE(IM,IN) = TEMP
C                                       Add to sum
                           ELSE IF (DOAVE) THEN
                              IMAGE(IM,IN) = IMAGE(IM,IN) + TEMP
C                                       Return minimum
                           ELSE IF (DOMIN) THEN
                              IMAGE(IM,IN) = MIN (IMAGE(IM,IN), TEMP)
C                                       Return Maximum
                           ELSE IF (DOMAX) THEN
                              IMAGE(IM,IN) = MAX (IMAGE(IM,IN), TEMP)
                              END IF
                           COUNT(IM,IN) = COUNT(IM,IN) + 1
                           END IF
                        END IF
 120                 CONTINUE
 130              CONTINUE
 140           CONTINUE
 150        CONTINUE
C                                       Do actual write whole plane
         DO 170 I = 1,IDY
C                                       The first write sets up indicies
            CALL MDISK ('WRIT', IDLUN, IDIND, RDBUFF, IDPOS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) IRET
               CALL MSGWRT (7)
               GO TO 999
               END IF
            DO 160 II = 1,IDX
C                                       average
               IF (COUNT(II,I).GT.0) THEN
                  IF (DOAVE) THEN
                     TEMP = IMAGE(II,I) / COUNT(II,I)
                  ELSE
                     TEMP = IMAGE(II,I)
                     END IF
                  RMAX = MAX (RMAX, TEMP)
                  RMIN = MIN (RMIN, TEMP)
               ELSE
                  TEMP = FBLANK
                  WASBLK = .TRUE.
                  END IF
               RDBUFF(IDPOS+II-1) = TEMP
 160           CONTINUE
 170        CONTINUE
C                                       Write last buffer.
         CALL MDISK ('FINI', IDLUN, IDIND, RDBUFF, IDPOS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1150) IRET
            CALL MSGWRT (7)
            GO TO 999
            END IF
 200     CONTINUE
C                                       Create new header.
C                                       Start with old header info.
      IF (RMAX.EQ.RMIN) THEN
         WRITE (MSGTXT,1200) RMAX
         CALL MSGWRT (6)
      ELSE IF (RMAX.LT.RMIN) THEN
         MSGTXT = 'NO VALID PIXELS FOUND'
         CALL MSGWRT (7)
         END IF
      CATR(KRDMN) = RMIN
      CATR(KRDMX) = RMAX
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('MINIT READ ERROR',I5)
 1110 FORMAT ('MINIT WRITE ERROR',I5)
 1115 FORMAT ('DOUBLE BUFFERED READ ERROR',I5)
 1120 FORMAT ('DOUBLE BUFFERED WRITE ERROR',I5)
 1150 FORMAT ('MDISK FINI ERROR',I5)
 1200 FORMAT ('WARNING: SUBIMAGE CONSTANT AT',1PE13.5)
      END
      SUBROUTINE SUBIMH
C-----------------------------------------------------------------------
C   copies tables, writes history
C-----------------------------------------------------------------------
C
      INCLUDE 'SUBIM.INC'
C
      INTEGER   IH1LUN, IH2LUN, NFILES, IERR
      CHARACTER NOTTYP*2, HILINE*72
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA IH1LUN, IH2LUN /27, 28/
      DATA NOTTYP /'  '/
      DATA NFILES /2/
C-----------------------------------------------------------------------
C                                       Initialize HITAB
      CALL HIINIT (NFILES)
C                                       Create and copy history file.
      CALL HISCOP (IH1LUN, IH2LUN, ISVOL, IDVOL, ISSLOT, IDSLOT,
     *   CATBLK, ISBUFF, IDBUFF, IERR)
      IF (IERR.GT.3) GO TO 300
      IF (IERR.EQ.3) GO TO 220
C                                       Add SUBIM history.
      CALL HENCO1 (TSKNAM, NAMIN, CLSIN, ISSEQ, ISVOL, IH2LUN,
     *   IDBUFF, IERR)
      IF (IERR.NE.0) GO TO 220
      WRITE (HILINE,1210) IMTYPE, NLUSER
      CALL HIADD (IH2LUN, HILINE, IDBUFF, IERR)
      IF (IERR.NE.0) GO TO 220
      CALL HENCOO (TSKNAM, NAMOUT, CLSOUT, IDSEQ, IDVOL, IH2LUN,
     *   IDBUFF, IERR)
      IF (IERR.NE.0) GO TO 220
      WRITE (HILINE,1211) ISWIN(1), ISWIN(2), INDX3L, INDX4L, INDX5L,
     *   INDX6L, INDX7L
      CALL HIADD (IH2LUN, HILINE, IDBUFF, IERR)
      IF (IERR.NE.0) GO TO 220
      WRITE (HILINE,1212) ISWIN(3), ISWIN(4), INDX3U, INDX4U, INDX5U,
     *   INDX6U, INDX7U
      CALL HIADD (IH2LUN, HILINE, IDBUFF, IERR)
      IF (IERR.NE.0) GO TO 220
      WRITE (HILINE,1213) IXINC
      CALL HIADD (IH2LUN, HILINE, IDBUFF, IERR)
      IF (IERR.NE.0) GO TO 220
      WRITE (HILINE,1214) IYINC
      CALL HIADD (IH2LUN, HILINE, IDBUFF, IERR)
      IF (IERR.NE.0) GO TO 220
      WRITE (HILINE,1215) IZINC
      CALL HIADD (IH2LUN, HILINE, IDBUFF, IERR)
      IF (IERR.NE.0) GO TO 220
      WRITE (HILINE,1216) OPCODE
      CALL HIADD (IH2LUN, HILINE, IDBUFF, IERR)
 220  CALL HICLOS (IH2LUN, .TRUE., IDBUFF, IERR)
C                                       Copy CC files and others
 300  CALL ALLTAB (0, NOTTYP, IH1LUN, IH2LUN, ISVOL, IDVOL, ISSLOT,
     *   IDSLOT, CATBLK, ISBUFF, IDBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1210 FORMAT ('SUBIM INTYPE =''',A2,'''',6X,'USERID=',I5)
 1211 FORMAT ('SUBIM BLC    =',2(I5,','),4(I5,','),I5)
 1212 FORMAT ('SUBIM TRC    =',2(I5,','),4(I5,','),I5)
 1213 FORMAT ('SUBIM XINC   =',I5)
 1214 FORMAT ('SUBIM YINC   =',I5)
 1215 FORMAT ('SUBIM ZINC   =',I5)
 1216 FORMAT ('SUBIM OPCODE = ''',A,'''')
      END
      SUBROUTINE SUBHDR (BLC, TRC, XINC, YINC, ZINC)
C-----------------------------------------------------------------------
C   SUBHDR corrects the header for subimaging: changes number of points
C   on the axes, the reference pixels, and the alternate axis (freq vs
C   velocity) reference pixel.  It corrects the first two axes for use
C   of pixel increments - namely the reference pixel and the axis incr.
C   Inputs:
C      BLC    R(7)        Bottom left corner to use
C      TRC    R(7)        Top right corner to use
C      XINC   R           Pixel increment on first axis
C      YINC   R           Pixel increment on second axis
C   Common:
C      /MAPHDR/ CATBLK     map header (in/out)
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), XINC, YINC, ZINC
C
      CHARACTER FCHARS(3)*4, CHTEMP*4
      HOLLERITH CATH(256)
      REAL      CATR(256), AINC(7)
      INTEGER   IPL, IPH, CATBLK(256), NAX, I, J
      DOUBLE PRECISION    CATD(128)
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Regular axis parameters
      NAX = CATBLK(KIDIM)
      CALL RFILL (7, 1.0, AINC)
      IF (XINC.GT.0.001) AINC(1) = XINC
      IF (YINC.GT.0.001) AINC(2) = YINC
      IF (ZINC.GT.0.001) AINC(3) = ZINC
      DO 10 I = 1,NAX
         IPL = BLC(I) + 0.01
         IPH = TRC(I) + 0.01
         CATBLK(KINAX+I-1) = (IPH - IPL) / AINC(I) + 1
         CATR(KRCRP+I-1) = (CATR(KRCRP+I-1) - IPL) / AINC(I) + 1.
         CATR(KRCIC+I-1) = CATR(KRCIC+I-1) * AINC(I)
 10      CONTINUE
C                                       Alternate axis
      IF (CATBLK(KIALT).NE.0) THEN
         DO 25 I = 1,NAX
            IPL = KHCTP + (I-1) * 2
            CALL H2CHR (4, 1, CATH(IPL), CHTEMP)
            DO 20 J = 1,3
C                                       Found one
               IF (FCHARS(J).EQ.CHTEMP) THEN
                  IPL = BLC(I) + 0.01
                  CATR(KRARP) = (CATR(KRARP) - IPL) / AINC(I) + 1.0
                  GO TO 999
                  END IF
 20            CONTINUE
 25         CONTINUE
         END IF
C
 999  RETURN
      END
