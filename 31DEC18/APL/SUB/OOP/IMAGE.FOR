C    Image Class Module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "Image" class library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2012, 2015
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
C    An image object consists of a pixel array member and a number of
C    descriptive classes.  General access to members is through member
C    functions IMGET and IMPUT.  Efficient access to pixel array data
C    are through ARREAD and ARRWRI.  Access may be by pixel, row, plane
C    or image. ARRCLO should be used to close the access if ARREAD or
C    ARRWRI are used.
C
C    Class public members:
C      ARRAY       R(*,*,*)      Array(s) of pixels
C      FILE_NAME   Base class    File name information
C      FILE_STATUS Base class    File status information
C      IMAGE_DESC  Base class    Descriptive information about the image
C      VELOCITY    Base class    Information for the conversion of
C                                frequency to velocity.
C      POSITION    Base class    Celestial position information
C      BEAM        Base class    Beam size / deconvolution information
C
C    Class private data:
C      IMGFVD   I(80,MAXIO)  FDV for ARRIO
C      IMGLRO   I(MAXIO)     Length of a row being transfered.
C
C   Public functions:
C     IMGCRE (name, ierr)
C        Creates an image object with name "name".
C     IMGDES (name, ierr)
C        Destroys the image object with name "name"; quasi-permanent
C        forms are unaffected.
C     IMGZAP (name, ierr)
C        Destroys the image object with name "name"; quasi-permanent
C        forms are deleted.
C     IMGCOP (namein, namout, ierr)
C        Copys one object to another.  The same quasi permanent forms
C        are used for both.
C     IMGCLN (namein, namout, ierr)
C        CLONES an object.  A new object is created and any associated
C        quasi-permanent forms are created.  The name, class etc. for
C        the output quasi-permanent catalog entries are given by
C        keywords OUTNAME, OUTCLASS, OUTSEQ and OUTDISK associated with
C        namein.  The output image will represent the specified subimage
C        in the input image.
C     IMGSCR (name, dim, ierr)
C        Creates an image scratch object of the size and structure given
C        by dim.
C     IMGOPN (name, status, ierr)
C        Opens an image object.  Checks for valid data.
C     IMGCLO (name, ierr)
C        Closes an image object.  Updates data validity.
C     IMGET (name, keywrd, type, dim, value, valuec, ierr)
C        Return keyword value.
C     IMPUT (name, keywrd, type, dim, value, valuec, ierr)
C        Store keyword value.
C     IMGATT (name, docrea, ierr)
C        Attach an AIPS catalog data file to an object.  The name, class
C        etc. for the output quasi-permanent catalog entries are given
C        by keywords NAME, CLASS, SEQ and DISK associated with NAME.
C        Creates the file if necessary.
C     IMCSET (name, status, ierr)
C        Sets any file status)
C     IMCCLR (name, ierr)
C        Clears any file status except destroy on fail.
C     IMCDES (name, status, ierr)
C        Clears destroy on fail status and resets file status.  If
C        status is blank no status is set.
C     IMGCVL (in1, in2, factor, out, ierr)
C        Convolves two images (in $QOOP/QIMAGE.FOR)
C     IMGADD (in1, in2, out, ierr)
C        Adds two image objects.
C     IMGSUB (in1, in2, out, ierr)
C        Subtracts in2 from in1 image objects
C     IMGMUL (in1, in2, out, ierr)
C        Multiplies in2 by in1 image objects
C     IMGDIV (in1, in2, out, ierr)
C        Divides in1 by in2 image objects
C     IMGNEG (in, out, ierr)
C        Negate the values of an image object.
C     IMGFFT (dir, in, out, ierr)
C        FFT an image
C     IMCOPY (in, out, ierr)
C        Copy one image to another.
C     IMGPAD (in, out, ierr)
C        Copy one image to another with zero padding.
C     IMGSCL (in, factor, out, ierr)
C        Scale an image with a factor.
C     FFTPAD (in, out, ierr)
C        Creates a scratch image suitable for FFTing an image and copies
C        the selected subset of the input image into the scratch image
C        with zero padding around the edges.  The scratch image is made
C        twice the size of the input image if possible.
C        (in $QOOP/QIMAGE.FOR)
C     IMGBSC (plus, minus, out, ierr)
C        Combine two beam-switched images, writing corrected image out.
C
C   Shared with derived classes
C     IMGCHK (in1, in2, ierr)
C        Checks that two images have compatible size and position.
C     IMGWIN (in1, blc, trc, naxis, ierr)
C        Determine specified window in an image.
C
C   Private functions:
C     CFLSET (name, disk, cno, status, ierr)
C        Set AIPS catalog status, DFIL.INC common
C     CFLCLR (name, disk, cno, status, ierr)
C        Clear AIPS catalog status, DFIL.INC common
C     IMCRET (name, ierr)
C        Creates file structures for image "name"
C     IMBGET (name, keywrd, type, dim, value, valuec, ierr)
C        Fetches member of a base class of image class
C     IMBPUT (name, keywrd, type, dim, value, valuec, ierr)
C        Stores member of a base class of image class
C     IMBTNF (catin, catout, ierr)
C       Copies table information from CATBLK catin to catout.
C     IMCLNX (namein, namout, ierr)
C       If NAMOUT not fully instantiated copies descriptors from NAMEIN
C       to NAMOUT allowing to subimaging in NAMEIN.
C-----------------------------------------------------------------------
LOCAL INCLUDE 'IMCLASS.INC'
C                                       Image class I/O
C                                       Requires OBJPARM.INC
      INTEGER   IMGFDV(80,MAXIO), IMGLRO(MAXIO)
      COMMON /IMGIOC/ IMGFDV, IMGLRO
LOCAL END
      SUBROUTINE IMGCRE (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates an image object with name "name"
C   Simple return if object exists.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER  IDIM(7), TBLC(7), OBJNUM, MSGSAV, DUMMY
      CHARACTER ACCESS*8, STATUS*4, DATYPE*8, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Does it exist already?
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OBNAME (NAME, OBJNUM, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.0) GO TO 999
C                                       Create AIPS object
      CALL OBCREA (NAME, 'IMAGE   ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default access = 'ROW'
      IDIM(1) = 8
      IDIM(2) = 1
      IDIM(3) = 0
      ACCESS = 'ROW'
      CALL ARPPUT (NAME, 'ACCESS', OOACAR, IDIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       File status init blank
      STATUS = '    '
      IDIM(1) = 4
      CALL FSTPUT (NAME, 'STATUS', OOACAR, IDIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default is valid.
      IDIM(1) = 1
      CALL FSTPUT (NAME, 'VALID', OOALOG, IDIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default, check labeling
      IDIM(1) = 1
      CALL IMDPUT (NAME, 'DOCHECK', OOALOG, IDIM, .FALSE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default datatype = REAL
      IDIM(1) = 8
      DATYPE = 'REAL'
      CALL ARDPUT (NAME, 'DATATYPE', OOACAR, IDIM, DUMMY, DATYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default window = all
      IDIM(1) = 7
      CALL FILL (7, 0, TBLC)
      CALL ARDPUT (NAME, 'BLC', OOAINT, IDIM, TBLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARDPUT (NAME, 'TRC', OOAINT, IDIM, TBLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE IMGDES (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the image object with name "name"; quasi-permanent forms
C   are unaffected.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL OBFREE (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE IMGZAP (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the image object with name "name"; quasi-permanent forms
C   are deleted.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      CHARACTER CNAME*12, CCLAS*6, PTYPE*2, STAT*4
      INTEGER   DISK, CNO, ITEMP, CAT(256), SEQ, USID, LOOP, WHICH
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find disk  and slot
      CALL OBDSKC (NAME, DISK, CNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get catalog header
      CALL OBHGET (NAME, CAT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       May have to clear status
C                                       (possibly several times)
 100     CALL CATDIR ('INFO', DISK, CNO, CNAME, CCLAS, SEQ, PTYPE, USID,
     *      STAT, SBUFF, IERR)
         IF (IERR.NE.0) GO TO 110
         IF (STAT.NE.'REST') THEN
            IF (STAT.EQ.'READ') STAT = 'CLRD'
            IF (STAT.EQ.'WRIT') STAT = 'CLWR'
            CALL CATDIR ('CSTA', DISK, CNO, CNAME,  CCLAS, SEQ, PTYPE,
     *         USID, STAT, SBUFF, IERR)
            IF (IERR.NE.0) GO TO 110
            GO TO 100
            END IF
C                                       Zap file
 110     CALL MDESTR (DISK, CNO, CAT, SBUFF, ITEMP, IERR)
C                                       Allow destroy to fail without
C                                       bombing the program.
C                                       Free Object slot
      CALL OBFREE (NAME, IERR)
C                                       Remove all traces in DFIL.
C                                       Catalog status marked
      WHICH = -1
      DO 200 LOOP = 1,NCFILE
         IF ((DISK.EQ.FVOL(LOOP)) .AND. (CNO.EQ.FCNO(LOOP))) THEN
            WHICH = LOOP
            FRW(LOOP) = -1
            END IF
 200     CONTINUE
      IF (WHICH.EQ.NCFILE) NCFILE = NCFILE - 1
C                                       Scratch file?
      WHICH = -1
      DO 300 LOOP = 1,NSCR
         IF ((DISK.EQ.SCRVOL(LOOP)) .AND. (CNO.EQ.SCRCNO(LOOP))) THEN
            WHICH = LOOP
            SCRVOL(LOOP) = -1
            SCRCNO(LOOP) = -1
            GO TO 310
            END IF
 300     CONTINUE
C                                       Compress list
 310  CALL CMPCFL
C
 999  RETURN
      END
      SUBROUTINE IMGOPN (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Sets up to open an image file.  Obtains header info etc.
C   For images to be written the validity of the data is checked
C   Inputs:
C      NAME   C*?   The name of the object.
C      STATUS C*4   'READ', 'WRIT', 'DEST' (write but destroy on
C                   failure).
C   Output:
C      IERR  I     Error return code, 0=OK, 5=data invalid
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INTEGER   DIM(7), TYPE
      LOGICAL   VALID, DOCRE
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Attach to file
      DOCRE = STATUS .NE. 'READ'
      CALL IMGATT (NAME, DOCRE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       AIPS status
      CALL IMCSET (NAME, STATUS, IERR)
C                                       For Read checks that file valid
      IF (STATUS.EQ.'READ') THEN
         CALL FSTGET (NAME, 'VALID', TYPE, DIM, VALID, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (.NOT.VALID) THEN
            MSGTXT = NAME // ' IMAGE INVALID'
            CALL MSGWRT(6)
            IERR = 5
            GO TO 999
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE IMGCLO (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Closes image updating disk resident information.  For files being
C   written, the validity of the data is set.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   CAT(256), DISK, CNO, DIM(7), TYPE, DUMMY
      CHARACTER STATUS*4, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup STATUS
      CALL FSTGET (NAME, 'STATUS', TYPE, DIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((STATUS.EQ.'WRIT') .OR. (STATUS.EQ.'DEST')) THEN
C                                       Get CATBLK
         CALL OBHGET (NAME, CAT, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Update AIPS catalog header for
C                                       write
C                                       Find disk and slot
         CALL OBDSKC (NAME, DISK, CNO, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Store  CATBLK
         CALL CATIO ('UPDT', DISK, CNO, CAT, 'REST', SBUFF, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            GO TO 999
            END IF
         IERR = 0
C                                       Mark as valid.
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL FSTPUT (NAME, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       AIPS status
      CALL IMCCLR (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close object
      CALL OBCLOS (NAME, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATIO ERROR ',I3,' UPDATING CATALOG')
      END
      SUBROUTINE IMGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Returns the dimensionality and value(s) associated with a given
C   keyword.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   The name of the keyword in form 'MEM1.MEM2...'
C   Outputs:
C      TYPE     I     Data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*?   Associated value (character)
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Is this a base class?
      CALL IMBGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
      IF (IERR.EQ.1) CALL OBGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE,
     *   VALUEC, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE IMPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Stores the value (array) associated with a given keyword.
C   A new non-virtual keyword will be created if necessary.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   The name of the keyword in form 'MEM1.MEM2...'
C      TYPE     I     Data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*?   Associated value (character)
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Is this a base class?
      CALL IMBPUT (NAME, KEYWRD, TYPE, DIM(1), VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
      IF (IERR.EQ.1) CALL OBPUT (OBJNUM, KEYWRD, TYPE, DIM, VALUE,
     *   VALUEC, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE IMGCOP (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copys one object to another.  The same quasi permanent forms are
C   used for both.
C   Inputs:
C      NAMEIN  C*?   The name of the input object.
C      NAMOUT  C*?   The name of the output object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*(*), NAMOUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL OBCOPY (NAMEIN, NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGATT (NAMOUT, .FALSE., IERR)
C
 999  RETURN
      END
      SUBROUTINE IMGCLN (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Clones an object.  A new object is created and any associated
C   quasi-permanent forms are created.  The name, class etc. for the
C   output quasi-permanent catalog entries are given by keywords
C   OUTNAME, OUTCLASS, OUTSEQ and OUTDISK associated with namein.  The
C   output image will represent the specified subimage in the input
C   image.
C   Inputs:
C      NAMEIN  C*?   The name of the input object.
C      NAMOUT  C*?   The name of the output object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*(*), NAMOUT*(*)
      INTEGER   IERR
C
      INTEGER  SEQO, SEQI, DISKO, DISKI, CNOI, DIM(7), BLC(7), TRC(7),
     *   NAXIS(7), LOOP, TYPE, DUMMY(1)
      CHARACTER NAMO*12, CLAO*6, NAMI*12, CLAI*6, DEFCLS*6, CHTEMP*20,
     *   CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Attach NAMEIN to files
      CALL IMGATT (NAMEIN, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy object
      CALL OBCOPY (NAMEIN, NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Make new files - get CATBLK
      CALL OBHGET (NAMEIN, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find old disk and slot
      CALL OBDSKC (NAMEIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get new name etc info.
      CALL IMGET (NAMEIN, 'OUTNAME ', TYPE, DIM, DUMMY, NAMO, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGET (NAMEIN, 'OUTCLASS', TYPE, DIM, DUMMY, CLAO, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGET (NAMEIN, 'OUTSEQ', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      SEQO = DUMMY(1)
      CALL FNAGET (NAMEIN, 'NAMCLSTY', TYPE, DIM, DUMMY, CHTEMP, IERR)
      IF (IERR.NE.0) GO TO 999
      NAMI = CHTEMP(1:12)
      CLAI = CHTEMP(13:18)
      CALL FNAGET (NAMEIN, 'IMSEQ', TYPE, DIM, SEQI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Deal with subimaging
      CALL IMGWIN (NAMEIN, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 100 LOOP = 1,KICTPN
         CATBLK(KINAX+LOOP-1) = NAXIS(LOOP)
 100     CONTINUE
      CALL FILL (7, 0, BLC)
      CALL FILL (7, 0, TRC)
      DIM(1) = 7
      DIM(2) = 1
      DIM(3) = 0
      CALL ARDPUT (NAMOUT, 'BLC', OOAINT, DIM, BLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARDPUT (NAMOUT, 'TRC', OOAINT, DIM, TRC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Put info in CATBLK
      DEFCLS = '      '
      CALL MAKOUT (NAMI, CLAI, SEQI, DEFCLS, NAMO, CLAO, SEQO)
      CALL CHR2H (12, NAMO, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAO, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQO
C                                       Save header
      CALL OBHPUT (NAMOUT, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set disk number
      CALL IMGET (NAMEIN, 'OUTDISK ', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DISKO = DUMMY(1)
      CALL IMPUT (NAMOUT, 'DISK', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save name etc.
      DIM(1) = LEN (NAMO)
      DIM(2) = 1
      CALL FNAPUT (NAMOUT, 'NAME', OOACAR, DIM, DUMMY, NAMO, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = LEN (CLAO)
      CALL FNAPUT (NAMOUT, 'CLASS', OOACAR, DIM, DUMMY, CLAO, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      CALL FNAPUT (NAMOUT, 'IMSEQ', OOAINT, DIM, SEQO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close NAMEIN
      CALL IMGCLO (NAMEIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy descriptors etc
      CALL IMCLNX (NAMEIN, NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 999
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE IMGSCR (NAME, NAXIS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates a scratch object with size and structure defined by dim.
C   Adds keyword 'SCRCNO' to NAME giving the CFIL.INC scratch file
C   number which may be neded by AIPS utility routines.
C   Inputs:
C      NAME    C*?   The name of the output object.
C      NAXIS   I(*)  The dimensionality of the desired scratch image; A
C                    dimension of 0 means that that and higher axes are
C                    not defined.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   NAXIS(*), IERR
C
      INTEGER  LOOP, NAX, SIZE, IDIM(7), BLC(7), TRC(7), CAT(256), TYPE,
     *   DUMMY(1)
      CHARACTER FTYPE*8, ACCESS*8, STATUS*4, DATYPE*8, NAMCLT*20,
     *   NAMO*12, CLAO*6, SFNAME*48, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find number of dimensions
      NAX = -1
      DO 10 LOOP = 1,7
         IF (NAXIS(LOOP).GT.0) NAX = LOOP
 10      CONTINUE
      IF (NAX.LE.0) THEN
         MSGTXT = 'ATTEMPT TO DEFINE SCRATCH IMAGE WITH NO AXES'
         IERR = 2
         GO TO 990
         END IF
C                                       Create object
      CALL OBCREA (NAME, 'IMAGE', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Size of scratch file
      CALL MAPSIZ (NAX, NAXIS, SIZE)
C                                       Create Scratch file.
      CALL SCREAT (SIZE, SBUFF, IERR)
C                                       Register in DFIL.INC
      IF ((IERR.EQ.0) .AND. (NCFILE.LT.FILIST)) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = SCRVOL(NSCR)
         FCNO(NCFILE) = SCRCNO(NSCR)
         FRW(NCFILE) = 2
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       Fetch  CATBLK
      CALL CATIO ('READ', SCRVOL(NSCR), SCRCNO(NSCR), CAT, 'REST',
     *   SBUFF, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      IERR = 0
C                                       Save header
      CALL OBHPUT (NAME, CAT, IERR)
      IF (IERR.NE.0) GO TO 999
      IDIM(1) = 1
      IDIM(2) = 1
      IDIM(3) = 0
C                                       Save Name info
      CALL FNAPUT (NAME, 'DISK', OOAINT, IDIM, SCRVOL(NSCR), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FNAPUT (NAME, 'CNO', OOAINT, IDIM, SCRCNO(NSCR), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       CFIL.INC scratch file number.
      DUMMY(1) = NSCR
      CALL IMPUT (NAME, 'SCRCNO', OOAINT, IDIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      FTYPE = 'AIPS'
      IDIM(1) = 8
      CALL FNAPUT (NAME, 'FTYPE', OOACAR, IDIM, DUMMY, FTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Name
      CALL FNAGET (NAME, 'NAMCLSTY', TYPE, IDIM, DUMMY, NAMCLT, IERR)
      IF (IERR.NE.0) GO TO 999
      NAMO = NAMCLT(1:12)
      CLAO = NAMCLT(13:18)
      IDIM(1) = 12
      CALL FNAPUT (NAME, 'NAME', OOACAR, IDIM, DUMMY, NAMO, IERR)
      IF (IERR.NE.0) GO TO 999
      IDIM(1) = 6
      CALL FNAPUT (NAME, 'CLASS', OOACAR, IDIM, DUMMY, CLAO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       File status init 'WRIT'
      STATUS = 'WRIT'
      IDIM(1) = 4
      CALL FSTPUT (NAME, 'STATUS', OOACAR, IDIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Mark as invalid.
      IDIM(1) = 1
      CALL FSTPUT (NAME, 'VALID', OOALOG, IDIM, .FALSE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save Array description
      IDIM(1) = 7
      CALL ARDPUT (NAME, 'NAXIS', OOAINT, IDIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FILL (7, 0, BLC)
      CALL ARDPUT (NAME, 'BLC', OOAINT, IDIM, BLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FILL (7, 0, TRC)
      CALL ARDPUT (NAME, 'TRC', OOAINT, IDIM, TRC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IDIM(1) = 1
      CALL ARDPUT (NAME, 'NDIM', OOAINT, IDIM, NAX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARDPUT (NAME, 'FDISK', OOAINT, IDIM, SCRVOL(NSCR), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, SFNAME, IERR)
      IDIM(1) = LEN (SFNAME)
      IERR = 0
      CALL ARDPUT (NAME, 'FNAME', OOACAR, IDIM, DUMMY, SFNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default access = 'ROW'
      IDIM(1) = 8
      ACCESS = 'ROW'
      CALL ARPPUT (NAME, 'ACCESS', OOACAR, IDIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Don't check labeling
      IDIM(1) = 1
      CALL IMDPUT (NAME, 'DOCHECK', OOALOG, IDIM, .FALSE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default datatype = REAL
      IDIM(1) = 8
      DATYPE = 'REAL'
      CALL ARDPUT (NAME, 'DATATYPE', OOACAR, IDIM, DUMMY, DATYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Store Modified header to disk
C                                       Fetch  CATBLK
      CALL OBHGET (NAME, CAT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CATIO ('UPDT', SCRVOL(NSCR), SCRCNO(NSCR), CAT, 'REST',
     *   SBUFF, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      IERR = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' READING SCRATCH FILE CATBLK')
      END
      SUBROUTINE IMGATT (NAME, DOCREA, IERR)
C-----------------------------------------------------------------------
C   Public
C   Attach an AIPS catalog data file to an object.  The name, class etc.
C   for the output quasi-permanent catalog entries are given by keywords
C   NAME, CLASS, SEQ and DISK associated with NAME.  Files will be
C   created if necessary if DOCREA=.true.
C   If DOCREA=.TRUE. and SEQ=0 then a new file will be created.
C   Inputs:
C      NAME   C*?   The name of the object.
C      DOCREA L     If true then create the file is it doesn't exist.
C   Output:
C      IERR   I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      LOGICAL   DOCREA
      INTEGER   IERR
C
      INTEGER   CAT(256), SEQI, DISKI, CNOI, DIM(7), TYPE, NDIMI, NDIMO,
     *   NAXISI(7), NAXISO(7), I, DUMMY(1), MSGSAV
      LOGICAL NUFILE
      CHARACTER NAMI*12, CLAI*6, PTYPE*2, STAT*4, FNAME*48, FTYPE*8,
     *   NAMCL*20, CTYPEI(7)*8, CTYPEO(7)*8, CDUMMY*1
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128), AVFREQ
      EQUIVALENCE (CAT, CATH, CATD)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
C                                       Get name etc info.
      CALL FNAGET (NAME, 'NAME', TYPE, DIM, DUMMY, NAMI, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FNAGET (NAME, 'CLASS', TYPE, DIM, DUMMY, CLAI, IERR)
      IF (IERR.NE.0) GO TO 999
      PTYPE = '  '
      CALL FNAGET (NAME, 'DISK', TYPE, DIM, DISKI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FNAGET (NAME, 'IMSEQ', TYPE, DIM, SEQI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Requested size and axes
      CALL ARDGET (NAME, 'NDIM', TYPE, DIM, NDIMI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARDGET (NAME, 'NAXIS', TYPE, DIM, NAXISI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMDGET (NAME, 'CTYPE', TYPE, DIM, DUMMY, CTYPEI, IERR)
      IF (IERR.NE.0) GO TO 999
      NUFILE = .FALSE.
C                                       Check for old one?
      IF (DOCREA .AND. (SEQI.LE.0)) THEN
C                                       Force new
         IERR = 5
      ELSE
C                                       Find file
         CNOI = 1
         CALL CATDIR ('SRCH', DISKI, CNOI, NAMI, CLAI, SEQI, PTYPE,
     *      NLUSER, STAT, SBUFF, IERR)
         END IF
C                                       If it doesn't exist create it
      IF ((IERR.EQ.5) .AND. DOCREA) THEN
C                                       Make sure name set.
         NAMCL = NAMI // CLAI // 'MA'
         DIM(1) = 20
         DIM(2) = 1
         CALL FNAPUT (NAME, 'NAMCLSTY', OOACAR, DIM, DUMMY, NAMCL, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Create
         CALL IMCRET (NAME, IERR)
         IF (IERR.NE.0) GO TO 999
         NUFILE = .TRUE.
         CNOI = 1
C                                       Try again.
         CALL CATDIR ('SRCH', DISKI, CNOI, NAMI, CLAI, SEQI, PTYPE,
     *      NLUSER, STAT, SBUFF, IERR)
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, NAMI, CLAI, SEQI, DISKI, NLUSER
         GO TO 990
         END IF
C                                       Damn catalog flags
      CALL CATIO ('READ', DISKI, CNOI, CAT, 'REST', SBUFF, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1001) IERR
         GO TO 990
         END IF
      IERR = 0
      MSGSUP = 32000
      CALL OGET (NAME, 'AVERFREQ', TYPE, DIM, AVFREQ, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.EQ.0) .AND. (AVFREQ.GT.0.0D0) .AND.
     *   (CATD(KDCRV+2).NE.AVFREQ)) THEN
         CATD(KDCRV+2) = AVFREQ
         CALL CATIO ('WRIT', DISKI, CNOI, CAT, 'REST', SBUFF, IERR)
         END IF
      IERR = 0
C                                       Save header
      CALL OBHPUT (NAME, CAT, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
C                                       Actual size and axes
      CALL ARDGET (NAME, 'NDIM', TYPE, DIM, NDIMO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARDGET (NAME, 'NAXIS', TYPE, DIM, NAXISO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMDGET (NAME, 'CTYPE', TYPE, DIM, DUMMY, CTYPEO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check compatability if created
      IF (.NOT.NUFILE .AND. DOCREA) THEN
         IF (NDIMI.NE.NDIMO) THEN
            WRITE (MSGTXT, 1002) NDIMI, NDIMO
            IERR = 1
            GO TO 990
            END IF
         DO 100 I = 1,NDIMI
C                                       Axis size
            IF (NAXISI(I).NE.NAXISO(I)) THEN
               WRITE (MSGTXT, 1003) I, NAXISI(I), NAXISO(I)
               IERR = 1
               GO TO 990
               END IF
C                                       Axis label
            IF (CTYPEI(I).NE.CTYPEO(I)) THEN
               WRITE (MSGTXT, 1004) I, CTYPEI(I), CTYPEO(I)
               IERR = 1
               GO TO 990
               END IF
 100        CONTINUE
         END IF
C                                       Save file information
      DIM(1) = 1
      DIM(2) = 1
      CALL FNAPUT (NAME, 'DISK', OOAINT, DIM, DISKI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FNAPUT (NAME, 'CNO', OOAINT, DIM, CNOI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      FTYPE = 'AIPS'
      DIM(1) = 8
      CALL FNAPUT (NAME, 'FTYPE', OOACAR, DIM, DUMMY, FTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save name etc info.
      DIM(1) = 12
      CALL FNAPUT (NAME, 'NAME', OOACAR, DIM, DUMMY, NAMI, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 6
      CALL FNAPUT (NAME, 'CLASS', OOACAR, DIM, DUMMY, CLAI, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save array name
      CALL ZPHFIL (PTYPE, DISKI, CNOI, 1, FNAME, IERR)
      DIM(1) = 48
      DIM(2) = 1
      DIM(3) = 0
      CALL ARDPUT (NAME, 'FNAME', OOACAR, DIM, DUMMY, FNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      CALL ARDPUT (NAME, 'FDISK', OOAINT, DIM, DISKI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
      MSGTXT = 'IMGATT: ERROR WITH IMAGE ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1001 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1002 FORMAT ('EXISTING IMAGE DIMENSIONALITY INCOMPATABLE', 2I3)
 1003 FORMAT ('EXISTING IMAGE AXIS ',I2,' SIZE INCOMPATABLE', 2I5)
 1004 FORMAT ('EXISTING IMAGE AXIS ',I2,' LABEL INCOMPATABLE ',
     *   A,1X,A)
      END
      SUBROUTINE IMCSET (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Sets file status
C   Inputs:
C      NAME     C*?   The name of the object.
C      STATUS   C*4   'READ', 'WRIT', 'DEST' (Write but destroy the file
C                     on error).
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4, OLDSTA*4
      INTEGER   IERR
C
      INTEGER   DISK, CNO, DIM(3), TYPE, DUMMY(1)
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup DISK, CNO
      CALL OBDSKC (NAME, DISK, CNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set AIPS commons.
      CALL CFLSET (NAME, DISK, CNO, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set status
      CALL FSTGET (NAME, 'STATUS', TYPE, DIM, DUMMY, OLDSTA, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Ignore if 'DEST'
      IF (OLDSTA.NE.'DEST') THEN
         CALL FSTPUT (NAME, 'STATUS', OOACAR, DIM, DUMMY, STATUS, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE IMCCLR (NAME, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Clears file status.  Any delete on fail status will be kept.
C   Inputs:
C      NAME     C*?   The name of the object.
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   DISK, CNO, DIM(3), TYPE, DUMMY(1)
      CHARACTER STATUS*4
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup STATUS
      CALL FSTGET (NAME, 'STATUS', TYPE, DIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Lookup DISK, CNO
      CALL OBDSKC (NAME, DISK, CNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Process Keyword
      CALL CFLCLR (NAME, DISK, CNO, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Return if STATUS='DEST'
      IF (STATUS.EQ.'DEST') GO TO 999
C                                       Reset status
      DIM(1) = 4
      DIM(2) = 1
      DIM(3) = 0
      STATUS = '    '
      CALL FSTPUT (NAME, 'STATUS', OOACAR, DIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE IMCDES (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Clears destroy on fail status and resets file status.  If
C   status is blank no status is set.
C   Inputs:
C      NAME     C*?   The name of the object.
C      STATUS   C*4   'READ', 'WRIT', 'DEST' (Write but destroy the file
C                     on error). '    ' => no status set.
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INTEGER   DIM(7), DUMMY(1)
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Clear old status
      DIM(1) = 4
      DIM(2) = 1
      DIM(3) = 0
      CALL FSTPUT (NAME, 'STATUS', OOACAR, DIM, DUMMY, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMCCLR (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set new status
      IF (STATUS.NE.'    ') THEN
         CALL IMCSET (NAME, STATUS, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE IMGADD (IN1, IN2, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Add two images and write a third.
C   Inputs:
C      IN1   C*?   The name of the first image object.
C      IN2   C*?   The name of the second image object.
C      OUT   C*?   The name of the output image object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN1*(*), IN2*(*), OUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'IMCLASS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Setup OUT if necessary
      CALL IMCLNX (IN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open Images
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN1, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN2, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check compatibility
      CALL IMGCHK (IN1, IN2, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCHK (IN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Process arrays
      CALL ARRADD (IN1, IN2, OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Images
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN2, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMGADD: FIRST INPUT ' // IN1
      CALL MSGWRT (7)
      MSGTXT = 'IMGADD: SECOND INPUT ' // IN2
      CALL MSGWRT (7)
      MSGTXT = 'IMGADD: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IMGSUB (IN1, IN2, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Subtracts in2 from in1 image objects
C   Inputs:
C      IN1   C*?   The name of the first image object.
C      IN2   C*?   The name of the second image object.
C      OUT   C*?   The name of the output image object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN1*(*), IN2*(*), OUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'IMCLASS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Setup OUT if necessary
      CALL IMCLNX (IN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open Images
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN1, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN2, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check compatibility
      CALL IMGCHK (IN1, IN2, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCHK (IN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Process arrays
      CALL ARRSUB (IN1, IN2, OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Images
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN2, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMGSUB: FIRST INPUT ' // IN1
      CALL MSGWRT (7)
      MSGTXT = 'IMGSUB: SECOND INPUT ' // IN2
      CALL MSGWRT (7)
      MSGTXT = 'IMGSUB: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IMGMUL (IN1, IN2, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Multiplies two images and write a third.
C   Inputs:
C      IN1   C*?   The name of the first image object.
C      IN2   C*?   The name of the second image object.
C      OUT   C*?   The name of the output image object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN1*(*), IN2*(*), OUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'IMCLASS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Setup OUT if necessary
      CALL IMCLNX (IN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open Images
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN1, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN2, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check compatibility
      CALL IMGCHK (IN1, IN2, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCHK (IN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Process arrays
      CALL ARRMUL (IN1, IN2, OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Images
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN2, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMGMUL: FIRST INPUT ' // IN1
      CALL MSGWRT (7)
      MSGTXT = 'IMGMUL: SECOND INPUT ' // IN2
      CALL MSGWRT (7)
      MSGTXT = 'IMGMUL: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IMGDIV (IN1, IN2, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Divides in1 by in2 image objects
C   Inputs:
C      IN1   C*?   The name of the first image object.
C      IN2   C*?   The name of the second image object.
C      OUT   C*?   The name of the output image object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN1*(*), IN2*(*), OUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'IMCLASS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Setup OUT if necessary
      CALL IMCLNX (IN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open Images
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN1, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN2, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check compatibility
      CALL IMGCHK (IN1, IN2, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCHK (IN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Process arrays
      CALL ARRDIV (IN1, IN2, OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Images
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN2, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMGDIV: FIRST INPUT ' // IN1
      CALL MSGWRT (7)
      MSGTXT = 'IMGDIV: SECOND INPUT ' // IN2
      CALL MSGWRT (7)
      MSGTXT = 'IMGDIV: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IMGNEG (IN, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Negate an image and write another.
C   Inputs:
C      IN    C*?   The name of the input image object.
C      OUT   C*?   The name of the output image object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'IMCLASS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Setup OUT if necessary
      CALL IMCLNX (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open Images
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check compatibility
      CALL IMGCHK (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Process arrays
      CALL ARRNEG (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Images
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMGNEG: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'IMGNEG: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IMGCHK (IN1, IN2, IERR)
C-----------------------------------------------------------------------
C   Private
C   Check that two images have the same structure and position
C   Inputs:
C      IN1   C*?   The name of the first image object.
C      IN2   C*?   The name of the second image object.
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN1*(*), IN2*(*)
      INTEGER   IERR
C
      INTEGER   DIM(7), NAXIS1(7), NAXIS2(7), BLC1(7), BLC2(7),
     *   TRC1(7), TRC2(7), LOOP, TYPE, DUMMY(1)
      REAL      CDELT1(7), CDELT2(7)
      LOGICAL   DOCHK1, DOCHK2
      DOUBLE PRECISION CRVAL1(7), CRVAL2(7)
      CHARACTER CTYPE1(7)*8, CTYPE2(7)*8, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Axis dimensions, allow for
C                                       subimaging.
      CALL IMGWIN (IN1, BLC1, TRC1, NAXIS1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGWIN (IN2, BLC2, TRC2, NAXIS2, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 100 LOOP = 1,7
         IF (NAXIS1(LOOP).NE.NAXIS2(LOOP)) THEN
            IERR = 1
            WRITE (MSGTXT,1000) LOOP, NAXIS1(LOOP), NAXIS2(LOOP)
            GO TO 990
            END IF
 100     CONTINUE
C                                       Check labels?
      CALL IMDGET (IN1, 'DOCHECK', TYPE, DIM, DOCHK1, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMDGET (IN2, 'DOCHECK', TYPE, DIM, DOCHK2, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (.NOT. (DOCHK1.AND.DOCHK2)) GO TO 999
C                                       Axis type
      CALL IMDGET (IN1, 'CTYPE', TYPE, DIM, DUMMY, CTYPE1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMDGET (IN2, 'CTYPE', TYPE, DIM, DUMMY, CTYPE2, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 200 LOOP = 1,7
         IF (CTYPE1(LOOP).NE.CTYPE2(LOOP)) THEN
            IERR = 1
            WRITE (MSGTXT,1100) LOOP, CTYPE1(LOOP), CTYPE2(LOOP)
            GO TO 990
            END IF
 200     CONTINUE
C                                       Axis position, first two axes
C                                       only.
      CALL IMDGET (IN1, 'CRVAL', TYPE, DIM, CRVAL1, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMDGET (IN2, 'CRVAL', TYPE, DIM, CRVAL2, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMDGET (IN1, 'CDELT', TYPE, DIM, CDELT1, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMDGET (IN2, 'CDELT', TYPE, DIM, CDELT2, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 300 LOOP = 1,2
         IF (ABS (CRVAL2(LOOP)-CRVAL1(LOOP)) .GT. ABS (CDELT1(LOOP)))
     *      THEN
            IERR = 1
            WRITE (MSGTXT,1200) LOOP, CRVAL1(LOOP), CRVAL2(LOOP)
            GO TO 990
            END IF
 300     CONTINUE
C                                       Axis increment
      DO 400 LOOP = 1,2
         IF (ABS (CDELT2(LOOP)-CDELT1(LOOP)) .GT.
     *      (0.1*ABS (CDELT1(LOOP)))) THEN
            IERR = 1
            WRITE (MSGTXT,1300) LOOP, CDELT1(LOOP), CDELT2(LOOP)
            GO TO 990
            END IF
 400     CONTINUE
C                                       Error
      GO TO 999
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMAGE AXES ',I3,' HAVE UNEQUAL SIZE ',2I5)
 1100 FORMAT ('IMAGE AXES TYPES ',I3,' ARE UNEQUAL ',A,',',A)
 1200 FORMAT ('IMAGE AXES COORDINATES ',I3,' ARE UNEQUAL ',2D17.8)
 1300 FORMAT ('IMAGE AXES INCREMENTS ',I3,' ARE UNEQUAL ',2E13.5)
      END
      SUBROUTINE IMGWIN (NAME, BLC, TRC, NAXIS, IERR)
C-----------------------------------------------------------------------
C   Private
C   Determine the specified window in an image
C   Inputs:
C      NAME  C*?   The name of the image object.
C   Output:
C      BLC   I(*)  Bottom left corner
C      TRC   I(*)  Top Right corner
C      NAXIS I(*)  Dimensions of window defined by BLC, TRC
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   BLC(*), TRC(*), NAXIS(*), IERR
C
      INTEGER   DIM(7), NAX(7), IBLC(7), ITRC(7), LOOP, TYPE
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Axis dimensions
      CALL ARDGET (NAME, 'NAXIS', TYPE, DIM, NAX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Specified BLC
      CALL ARDGET (NAME, 'BLC', TYPE, DIM, IBLC, CDUMMY, IERR)
      IF (IERR.NE.0) CALL FILL (7, 0, IBLC)
C                                       Specified TRC
      CALL ARDGET (NAME, 'TRC', TYPE, DIM, ITRC, CDUMMY, IERR)
      IF (IERR.NE.0) CALL FILL (7, 0, ITRC)
C                                       Fill BLC, TRC
      DO 100 LOOP = 1,7
         BLC(LOOP) = MAX (1, IBLC(LOOP))
         BLC(LOOP) = MIN (BLC(LOOP), NAX(LOOP))
         TRC(LOOP) = ITRC(LOOP)
         IF (TRC(LOOP).LE.0) TRC(LOOP) = NAX(LOOP)
         TRC(LOOP) = MIN (TRC(LOOP), NAX(LOOP))
         NAXIS(LOOP) = MAX (1, (TRC(LOOP) - BLC(LOOP) + 1))
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CFLSET (NAME, DISK, CNO, STATUS, IERR)
C-----------------------------------------------------------------------
C   Private
C   Sets catalog status.  If the specified DISK and CNO already have an
C   AIPS catalog flag set then no action is taken.
C   Inputs:
C      NAME    C*? Object name
C      DISK    I   Disk number
C      CNO     I   Catalog status number
C      STATUS  C*4 'READ', 'WRIT', 'DEST' (Write but destroy the file
C                  on error).
C   Output:
C      IERR    I   Error return code, 0=OK, Otherwise failed.
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   DISK, CNO, IERR
C
      INTEGER   SEQI, BUFF(256), DIM(7), TYPE, INDEX, DUMMY(1)
      CHARACTER NAMI*12, CLAI*6, PTYPE*2, STAT*4, OLDSTA*4
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Catalog flag already set?
      CALL OBACF (DISK, CNO, INDEX)
      IF (INDEX.GT.0) GO TO 999
C                                       Check old status
      CALL FSTGET (NAME, 'STATUS', TYPE, DIM, DUMMY, OLDSTA, IERR)
      IF (IERR.NE.0) OLDSTA = ' '
      IERR = 0
C                                       If delete-on-fail write
C                                       status set simply return.
      IF (OLDSTA.EQ.'DEST') GO TO 999
C                                       Change status if old status is
C                                       blank.
      IF (OLDSTA.EQ.'    ') THEN
         STAT = STATUS
         IF (STATUS.EQ.'DEST') STAT = 'WRIT'
         CALL CATDIR ('CSTA', DISK, CNO, NAMI, CLAI, SEQI, PTYPE,
     *      NLUSER, STAT, BUFF, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.9) .AND. (IERR.NE.10)) THEN
            WRITE (MSGTXT,1000) IERR, STATUS
            GO TO 990
            END IF
C                                       Enter into DFIL.INC
         IERR = 0
         IF (NCFILE.LT.FILIST) THEN
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISK
            FCNO(NCFILE) = CNO
            FRW(NCFILE) = 0
            IF (STATUS.EQ.'READ') FRW(NCFILE) = 0
            IF (STATUS.EQ.'WRIT') FRW(NCFILE) = 1
            IF (STATUS.EQ.'DEST') FRW(NCFILE) = 2
         ELSE
            MSGTXT = 'INTERNAL TABLE FULL: CANNOT REMEMBER MORE '//
     *         'CATALOG FLAGS'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Error
      GO TO 999
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' CHANGING CATALOG STATUS TO ',A)
      END
      SUBROUTINE CFLCLR (NAME, DISK, CNO, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Clears file status. If DISK and CNO are not entered in the DFIL.INC
C   common then no action is taken.
C   Inputs:
C      NAME    C*? Object name
C      DISK    I   Disk number
C      CNO     I   Catalog status number
C      STATUS  C*4 'READ', 'WRIT', 'DEST' (Write but destroy the file
C                  on error).
C   Output:
C      IERR    I   Error return code, 0=OK, Otherwise failed.
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   DISK, CNO, IERR
C
      INTEGER   SEQI, BUFF(256), LOOP, IRW, HIGH, INDEX
      CHARACTER STATUS*4, NAMI*12, CLAI*6, PTYPE*2, STAT*4
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Catalog flag set?
      CALL OBACF (DISK, CNO, INDEX)
      IF (INDEX.LE.0) GO TO 999
C                                       Check status
C                                       If delete-on-fail write
C                                       status set simply return.
      IF (STATUS.EQ.'DEST') GO TO 999
C                                       Change status
      STAT = 'CLWR'
      IF (STATUS.EQ.'READ') STAT = 'CLRD'
      CALL CATDIR ('CSTA', DISK, CNO, NAMI, CLAI, SEQI, PTYPE,
     *   NLUSER, STAT, BUFF, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.9) .AND. (IERR.NE.10)) THEN
         WRITE (MSGTXT,1000) IERR, STAT
         GO TO 990
         END IF
C                                       CLear info in DFIL.INC
      IERR = 0
      IRW = 0
      IF (STATUS.EQ.'WRIT') IRW = 1
      IF (STATUS.EQ.'DEST') IRW = 2
      HIGH = NCFILE + 10
      DO 100 LOOP = 1,NCFILE
         IF ((FVOL(LOOP).EQ.DISK) .AND. (FCNO(LOOP).EQ.CNO)) THEN
            IF ((IRW.EQ.FRW(LOOP)) .OR. ((IRW.EQ.1) .AND.
     *         FRW(LOOP).EQ.2)) THEN
               FVOL(LOOP) = 0
               FCNO(LOOP) = 0
               FRW(LOOP) = -1
               HIGH = LOOP
               GO TO 110
               END IF
            END IF
 100     CONTINUE
C                                       Not found - quit
      GO TO 999
C                                       Compress DFIL.INC arrays
 110  CALL CMPCFL
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' CHANGING CATALOG STATUS TO ',A)
      END
      SUBROUTINE IMCOPY (IN, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copy one image to another.
C   Inputs:
C      IN    C*?   The name of the input image image.
C      OUT   C*?   The name of the output image image.
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'IMCLASS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Setup OUT if necessary
      CALL IMCLNX (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open Images
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Process arrays
      CALL ARRCOP (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Images
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMCOPY: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'IMCOPY: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IMGPAD (IN, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copy one image to another zero padding pixels outside of the input
C   image.
C   Inputs:
C      IN    C*?   The name of the input image image.
C      OUT   C*?   The name of the output image image.
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'IMCLASS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open Images
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Process arrays
      CALL ARRPAD (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Images
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMGPAD: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'IMGPAD: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IMGSCL (IN, FACTOR, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Scale an image with a constant factor.
C   Inputs:
C      IN     C*?   The name of the input image object.
C      FACTOR R     Scaling factor
C      OUT    C*?   The name of the output image object.
C   Output:
C      IERR   I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      REAL      FACTOR
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'IMCLASS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Setup OUT if necessary
      CALL IMCLNX (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open Images
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Process arrays
      CALL ARRSCL (IN, FACTOR, OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Images
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMGSCL: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'IMGSCL: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IMGBSC (PLUS, MINUS, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Combine two beam-switched images, writing corrected image out.
C   Inputs:
C      PLUS    C*?   The name of the plus throw image object.
C      MINUS   C*?   The name of the minus throw image object.
C      OUT     C*?   The name of the output image object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER PLUS*(*), MINUS*(*), OUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'IMCLASS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Setup OUT if necessary
      CALL IMCLNX (PLUS, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open Images
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (PLUS, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (MINUS, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check compatibility
      CALL IMGCHK (PLUS, MINUS, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCHK (PLUS, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Process arrays
      CALL ARRBSC (PLUS, MINUS, OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Images
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (PLUS, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (MINUS, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMGBSC: PLUS INPUT ' // PLUS
      CALL MSGWRT (7)
      MSGTXT = 'IMGBSC: MINUS INPUT ' // MINUS
      CALL MSGWRT (7)
      MSGTXT = 'IMGBSC: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IMCRET (NAME, IERR)
C-----------------------------------------------------------------------
C   Private
C   Creates a file structure for image "name".  Uses Name and header
C   associated with "name"
C   Inputs:
C      NAME    C*?   The name of the object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER  SEQO, DISKO, CNOO, DIM(7), TYPE, DUMMY(1)
      LOGICAL   EXISTS
      CHARACTER STATUS*4, NAMCLT*20, NAMO*12, CLAO*6, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get header
      CALL OBHGET (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Reset relevant information
      CATBLK(KINIT) = 0
      CATBLK(KITYP) = 0
      CATBLK(KRBMJ) = 0.0
      CATBLK(KRBMN) = 0.0
      CATBLK(KRBPA) = 0.0
C                                       Get disk number
      CALL FNAGET (NAME, 'DISK', TYPE, DIM, DISKO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create file.
      CNOO = 1
      STATUS = 'DEST'
      CALL MCREAT (DISKO, CNOO, SBUFF, IERR)
C                                       Register in DFIL.INC
      IF ((IERR.EQ.0) .AND. (NCFILE.LT.FILIST)) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = CNOO
         FRW(NCFILE) = 2
         END IF
C                                       Does it exist?
      EXISTS = IERR.EQ.2
      IF (EXISTS) IERR = 0
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1001) IERR
         GO TO 990
         END IF
      IF (EXISTS) THEN
         STATUS = 'WRIT'
C                                       Old get header
         CALL CATIO ('READ', DISKO, CNOO, CATBLK, 'REST', SBUFF, IERR)
C                                       Damn catalog flags
         IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
            WRITE (MSGTXT,1003) IERR
            GO TO 990
            END IF
         IERR = 0
         END IF
C                                       New save header
      CALL OBHPUT (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save CNO, actual DISK
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL FNAPUT (NAME, 'DISK', OOAINT, DIM, DISKO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      SEQO = CATBLK(KIIMS)
      CALL FNAPUT (NAME, 'IMSEQ', OOAINT, DIM, SEQO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FNAPUT (NAME, 'CNO', OOAINT, DIM, CNOO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Name
      CALL FNAGET (NAME, 'NAMCLSTY', TYPE, DIM, DUMMY, NAMCLT, IERR)
      IF (IERR.NE.0) GO TO 999
      NAMO = NAMCLT(1:12)
      CLAO = NAMCLT(13:18)
      DIM(1) = 12
      CALL FNAPUT (NAME, 'NAME', OOACAR, DIM, DUMMY, NAMO, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 6
      CALL FNAPUT (NAME, 'CLASS', OOACAR, DIM, DUMMY, CLAO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       File status init 'WRIT'
      STATUS = 'WRIT'
      DIM(1) = 4
      CALL FSTPUT (NAME, 'STATUS', OOACAR, DIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clear File status (undo MCREAT
C                                       status)
      CALL IMCCLR (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set destroy on fail flag
      STATUS = 'DEST'
      IF (EXISTS) STATUS = 'WRIT'
C???      CALL IMCSET (NAME, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Mark as invalid.
      DIM(1) = 1
      CALL FSTPUT (NAME, 'VALID', OOALOG, DIM, .FALSE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (7)
      MSGTXT = 'IMCRET: ERROR CREATING IMAGE ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('IMCRET: ERROR',I3,' CREATING OUTPUT FILE')
 1003 FORMAT ('IMCRET: ERROR',I3,' READING OLD CATBLK')
      END
      SUBROUTINE IMBGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Private
C   If KEYWRD refers to a recognized member base class then fetch the
C   value (array) for a specified member
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of value, an axis dimension of zero
C                    means that that dimension and higher are
C                    undefined.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find., 2=Failed
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
C
      INTEGER   POINT, NMEMS, IMEM, LOOP, JERR
      PARAMETER (NMEMS = 7)
      CHARACTER MEMBER*16, MEMS(NMEMS)*16
      INCLUDE 'INCS:DMSG.INC'
      DATA MEMS /'ARRAY', 'FILE_NAME', 'FILE_STATUS',  'IMAGE_DESC',
     *   'VELOCITY', 'POSITION', 'BEAM'/
C-----------------------------------------------------------------------
      IERR = 1
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized base
C                                       classes.
      IMEM = -1
      DO 10 LOOP = 1,NMEMS
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       If no base class and member not
C                                       recognized then return.
      IF ((IMEM.LE.0) .AND. (POINT.LE.0)) GO TO 999
C                                       Find it?, if not, complain and
C                                       die.
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNKNOWN MEMBER: ' // MEMBER // ' IN ' // NAME
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Call appropriate access
C                                       function:
      GO TO (110, 120, 130, 140, 150, 160, 170),  IMEM
C                                       ARRAY
 110     CALL ARRGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       File name
 120     CALL FNAGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       File Status
 130     CALL FSTGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       Image description
 140     CALL IMDGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       Velocity
 150     CALL VELGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       Position
 160     CALL PSNGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       Table
 170     CALL BEMGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
C
 900  IERR = 0
      IF (JERR.NE.0) IERR = 2
C
 999  RETURN
      END
      SUBROUTINE IMBPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Private
C   If KEYWRD refers to a recognized member base class then store the
C   value (array) for a specified member
C   Inputs:
C      NAME    C*?   The name of the object.
C      KEYWRD  C*?   Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of value, an axis dimension of zero
C                    means that that dimension and higher are
C                    undefined.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM, VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
C
      INTEGER   POINT, NMEMS, IMEM, LOOP, JERR
      PARAMETER (NMEMS = 7)
      CHARACTER MEMBER*16, MEMS(NMEMS)*16
      INCLUDE 'INCS:DMSG.INC'
      DATA MEMS /'ARRAY', 'FILE_NAME', 'FILE_STATUS',  'IMAGE_DESC',
     *   'VELOCITY', 'POSITION', 'BEAM'/
C-----------------------------------------------------------------------
      IERR = 1
C                                       Save member name
      POINT = INDEX (KEYWRD, '.')
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized base
C                                       classes.
      IMEM = -1
      DO 10 LOOP = 1,NMEMS
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       If no base class and member not
C                                       recognized then return.
      IF ((IMEM.LE.0) .AND. (POINT.LE.0)) GO TO 999
C                                       Find it?, if not, complain and
C                                       die.
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNKNOWN MEMBER: ' // MEMBER // ' IN ' // NAME
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Call appropriate access
C                                       function:
      GO TO (110, 120, 130, 140, 150, 160, 170), IMEM
C                                       ARRAY
 110     CALL ARRPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       File name
 120     CALL FNAPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       File Status
 130     CALL FSTPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       Image description
 140     CALL IMDPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       Velocity
 150     CALL VELPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       Position
 160     CALL PSNPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       Table
 170     CALL BEMPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
C
 900  IERR = 0
      IF (JERR.NE.0) IERR = 2
C
 999  RETURN
      END
      SUBROUTINE IMBTNF (CATIN, CATOUT, IERR)
C-----------------------------------------------------------------------
C   Image data utility routine
C   Updates table information from CATBLK  to CATOUT.  Existing table in
C   CATOUT will be preserved.
C   Inputs:
C      CATIN    I(256)  Input CATBLK with table info.
C   Output:
C      CATOUT   I(256)  Output CATBLK with table info added.
C      IERR     I       Error code, 0=>OK, else too many table types
C-----------------------------------------------------------------------
      INTEGER   CATIN(256), CATOUT(256), IERR
C
      INCLUDE 'INCS:PHDR.INC'
      INTEGER   LOOP, I, NUMOUT(NIEXTN), NOUT, N
      CHARACTER TYPOUT(NIEXTN)*2, TYPE
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       See what's already in output.
      NOUT = 0
      CALL FXHDEX (CATOUT)
      DO 100 LOOP = 1,KIEXTN
         N = CATOUT(KIVER+LOOP-1)
         CALL H2CHR (2, 1, CATOUT(KHEXT+LOOP-1), TYPE)
         IF ((N.GT.0) .AND. (TYPE.NE.'  ')) THEN
            NOUT = NOUT + 1
            NUMOUT(LOOP) = N
            TYPOUT(LOOP) = TYPE
            END IF
 100     CONTINUE
C                                       Loop over possible new
C                                       table/extension file types
      CALL FXHDEX (CATIN)
      DO 200 LOOP = 1,KIEXTN
         N = CATIN(KIVER+LOOP-1)
         CALL H2CHR (2, 1, CATIN(KHEXT+LOOP-1), TYPE)
         IF ((N.GT.0) .AND. (TYPE.NE.'  ')) THEN
C                                       Already have this type?
            DO 150 I = 1,NOUT
               IF (TYPE.EQ.TYPOUT(I)) THEN
C                                       Yes, add new version if
C                                       necessary.
                  CATOUT(KIVER+I-1) = MAX (N, NUMOUT(I))
                  GO TO 200
                  END IF
 150           CONTINUE
C                                       Add new entry
            IF (NOUT.LT.NIEXTN) THEN
               NOUT = NOUT + 1
               CATOUT(KIVER+NOUT-1) = CATIN(KIVER+LOOP-1)
               CATOUT(KHEXT+NOUT-1) = CATIN(KHEXT+LOOP-1)
            ELSE
               MSGTXT = 'IMBTNF: TOO MANY TABLE TYPES'
               CALL MSGWRT (6)
               IERR = 1
               END IF
            END IF
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE IMCLNX (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Private
C   Copys descriptors from NAMEIN to NAMOUT if NAMOUT is not fully
C   instantiated (AIPS files exist).  The array dimensions of NAMOUT are
C   adjusted to take into account BLC and TRC in NAMEIN.
C   Copies any catalog header keywords.
C   Inputs:
C      NAMEIN  C*?   The name of the input object.
C      NAMOUT  C*?   The name of the output object.  Must have naming
C                    set but need not be fully instantiated.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*(*), NAMOUT*(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), NAXIS(7), TYPE, DIM(3), I, IDISK, ICNO,
     *   ODISK, OCNO, SEQO
      REAL      CRPIXI(7), CRPIXO(7)
      LOGICAL   EXIST
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Does NAMOUT exist?
      CALL FNAGET (NAMOUT, 'IMSEQ', TYPE, DIM, SEQO, CDUMMY, IERR)
      IF ((IERR.NE.0) .OR. (SEQO.GT.0)) THEN
         CALL OBFEXS (NAMOUT, EXIST, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) GO TO 999
         END IF
C                                       Open input to be sure it's fully
C                                       instantiated.
      CALL IMGOPN (NAMEIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (NAMEIN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy descriptors
      CALL IMDCOP (NAMEIN, NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARDCOP (NAMEIN, NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Adjust for windowing in NAMEIN
      CALL ARRWIN (NAMEIN, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 990
      DIM(1) = 7
      DIM(2) = 1
      DIM(3) = 0
      CALL ARDPUT (NAMOUT, 'NAXIS', OOAINT, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Adjust reference pixel if
C                                       necessary.
      CALL IMDGET (NAMEIN, 'CRPIX', TYPE, DIM, CRPIXI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      DO 100 I = 1,7
         CRPIXO(I) = CRPIXI(I) - BLC(I) + 1
 100     CONTINUE
      CALL IMDPUT (NAMOUT, 'CRPIX', OOARE, DIM, CRPIXO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset BLC, TRC
      CALL FILL (7, 1, BLC)
      CALL ARDPUT (NAMOUT, 'BLC', OOAINT, DIM, BLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL COPY (7, NAXIS, TRC)
      CALL ARDPUT (NAMOUT, 'TRC', OOAINT, DIM, TRC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open to fully instantiate
      CALL IMGOPN (NAMOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy catalog header keywords.
      CALL OBDSKC (NAMEIN, IDISK, ICNO, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBDSKC (NAMOUT, ODISK, OCNO, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL KEYCOP (IDISK, ICNO, ODISK, OCNO, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error.
 990  MSGTXT = 'IMCLNX: ERROR SETTING DESCRIPTORS FOR ' // NAMOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
