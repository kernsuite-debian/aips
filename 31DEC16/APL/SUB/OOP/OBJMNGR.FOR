C   Object Manager Module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran Object manager library
C# Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2005, 2015
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
C   This version of the Object manager uses c language routines to
C   allocate and manage the linked list and CATBLK storage for each
C   object.
C   Private data:
C     MAXCLS   PI           Number of classes defined.
C     MAXVKW   PI           Maximum number of virtual keywords.
C     VKWCLS   H*8(MAXCLS)  Class name for virtual keyword.
C     VKWNUM   I(MAXCLS)    Number of defined virtual keywords.
C     VKWTAB   I(7,MAXVKW,MAXCLS) Virtual keyword information array.
C                           The second dimension is per keyword and the
C                           third is per class.  The structure of the
C                           first dimension is:
C          Word  Type        Contents
C          ----  ----  --------------------------------------------
C           1     H     Keyword
C           3     I     Category
C           4     I     Pointer
C           5     I     Data type
C           6     I     Dim(1)
C           7     I     Dim(2)
C    Where
C    keyword:   name of the virtual keyword as HOLLERITH (2 words = 8
C               char)
C    category:  1 = in fixed portion of catalog header, pointer is
C               pointer into type dependent array.  D values must be
C               copied from R array
C               2 = in keyword/value portion of catalog header, some
C               restrictions apply (not more than 2 words of data).
C               3 = Special derived keywords read access only.  Pointer
C               specifies a class specific function.
C     pointer:  pointer to catalog header entry or function.
C     data type: 1,2,3,4,5 for D, R, C, I, L data types of associated
C               data.
C     dim       Dimensionality of value, an axis dimension of zero
C               means that that dimension and higher are undefined.  For
C               character strings the length of the string is the first
C               dimension.
C
C     VKTKEY   PI                VKWTAB col. number for keyword
C     VKTCAT   PI                VKWTAB col. number for category
C     VKTPNT   PI                VKWTAB col. number for pointer
C     VKTYPE   PI                VKWTAB col. number for data type
C     VKTDM1   PI                VKWTAB col. number for dimension 1
C     VKTDM2   PI                VKWTAB col. number for dimension 2
C
C   Shared data with Class I/O (CLASSIO.INC)
C      MAXIO   PI              Maximum number of I/O simultaneous
C                              streams
C      BUFSIZ  PI              Buffer size in words
C      OBUFFR  R(BUFSIZ,MAXIO) I/O buffers
C      BUFPNT  I(MAXIO)        Buffer pointer
C      OBJLUN  I(MAXIO)        LUNs for I/O
C      OBJFIN  I(MAXIO)        FTAB pointer for I/O
C      LUNUSE  L(100)          True if LUN=index allocated.
C
C   Public functions:
C      OBINIT (ierr)
C         Initialize Object manager.
C      OBVHKW (class, keyword, type, ierr)
C         Add a Catalog header keyword to the virtual keyword list.
C
C   Private functions:
C     INVINI (ierr)
C        Initialize virtual keywords for inputs class.
C     IMVINI (ierr)
C        Initialize virtual keywords for image class.
C     OBKEYV (objnum, keywrd, keypnt, ierr)
C        See if keyword is an object dependent, virtual keyword.
C     OBRGET (objnum, keywrd, type, dim, value, valuec, ierr)
C        Fetch the value (array) for a specified real (non-virtual)
C        keyword.
C
C   Shared functions with class modules:
C     OBCREA (name, class, ierr)
C        Associate an object slot with an object name.
C     OBFREE (name, ierr)
C        Free the object slot associated with an object.
C     OBNAME (name, objnum, ierr)
C        Look up the object slot number of object with name "name".
C     OBCLAS (obnum, clasno, name, ierr)
C        Look up the class number of object number objnum.
C     OBPUT (objnum, keywrd, type, dim, value, valuec, ierr)
C        Save an entry in an object creating it if necessary.
C     OBGET (objnum, keywrd, type, dim, value, valuec, ierr)
C        Fetch the value (array) for a specified keyword.
C     OBLUN (lun, ierr)
C        Find a free LUN.
C     OBLUFR (lun)
C        Releases an LUN
C     OBINFO (name, bufno, ierr)
C        Look up I/O stream associated with an object.
C     OBDSKC (name, disk, cno, ierr)
C        Return Disk and slot information for object.
C     OBHGET (name, cat, ierr)
C        Return catalog header record for an object.
C     OBHPUT (name, cat, ierr)
C        Store catalog header record for an object.
C     OBCOPY (namein, namout, ierr)
C        Copies one image to another.
C     OBOPEN (name, ierr)
C        Assigns a buffer.
C     OBCLOS (name, ierr)
C        Closes a buffer associated with an object.
C     OBC2H (string, dim1, dim2, holl)
C        Convert a string (possibly an array) to hollerith
C     OBH2C (holl, dim1, dim2, string)
C        Convert a hollerith (possibly an array) to a string.
C-----------------------------------------------------------------------
LOCAL INCLUDE 'OBJECT.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
LOCAL END
LOCAL INCLUDE 'VIRTKEYW.INC'
C                                       Virtual Keyword common
      INCLUDE 'INCS:OBJPARM.INC'
      INTEGER   VKWNUM(MAXCLS), VKWTAB(7,MAXVKW,MAXCLS)
      HOLLERITH VKWTAH(7,MAXVKW,MAXCLS)
      CHARACTER VKWCLS(MAXCLS)*8
      EQUIVALENCE (VKWTAB, VKWTAH)
      COMMON /VIRKEY/ VKWNUM, VKWTAB
      COMMON /VIRKEC/ VKWCLS
C                                       Column pointers in VKWTAB
      INTEGER   VKTKEY, VKTCAT, VKTPNT, VKTYPE, VKTDM1, VKTDM2
C                                       VKTKEY = KEYWORD (H)
      PARAMETER (VKTKEY = 1)
C                                       VKTCAT = category
      PARAMETER (VKTCAT = 3)
C                                       VKTPNT = pointer
      PARAMETER (VKTPNT = 4)
C                                       VKTYPE = data type
      PARAMETER (VKTYPE = 5)
C                                       VKTDM1 = dimension 1
      PARAMETER (VKTDM1 = 6)
C                                       VKTDM2 = dimension 2
      PARAMETER (VKTDM2 = 7)
LOCAL END
      SUBROUTINE OBINIT (IERR)
C-----------------------------------------------------------------------
C   Public function
C   Initialize Object manager and class I/O.
C   Outputs:
C      IERR   I  Return error code 0=>OK else failed to create class.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LOOP
      INCLUDE 'OBJECT.INC'
      INCLUDE 'VIRTKEYW.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Initialize catalog pointers
      CALL VHDRIN
C                                       Init object information.
      CALL ZOINTD
C                                       Init I/O
      DO 100 LOOP = 1,MAXIO
C                                       Stream/buffer in use flag
         OBNUSE(LOOP) = .FALSE.
C                                       LUN
         OBJLUN(LOOP) = 0
C                                       FTAB pointer
         OBJFIN(LOOP) = 0
C                                       Buffer pointer
         BUFPNT(LOOP) = 0
C                                       Object associated with buffer
         BUFNAM(LOOP) = ' '
 100     CONTINUE
C                                       Init class information.
C                                       Virtual keywords.
      DO 150 LOOP = 1,MAXCLS
C                                       Number of keywords
         VKWNUM(LOOP) = 0
C                                       Class names
         VKWCLS(LOOP) = '        '
 150     CONTINUE
C                                       Input class (none)
      CALL INVINI (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Image class
      CALL IMVINI (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       UV data class
      CALL UVVINI (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Internal flags if LUN used.
      DO 300 LOOP = 1,128
         LUNUSE(LOOP) = .FALSE.
 300     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE OBVHKW (CLASS, KEYWRD, TYPE, IERR)
C-----------------------------------------------------------------------
C   Public
C   Declare a task specific catalog header keyword.  This is only useful
C   for classes with underlying disk data structures.
C   Inputs:
C      CLASS   C*8   Object class
C      KEYWRD  C*8   Keyword
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C   Input/Output in common:
C      VKWCLS  H*8(MAXCLS)  Class name for virtual keyword.
C      VKWNUM  I(MAXCLS)    Number of defined virtual keywords.
C      VKWTAB  I(7,MAXVKW,MAXCLS) Virtual keyword information array.
C   Outputs:
C      IERR   I      Error code, 0=OK. 1 = table full, 2 =class not
C                    defined.
C-----------------------------------------------------------------------
      CHARACTER CLASS*8, KEYWRD*8
      INTEGER   TYPE, IERR
C
      INTEGER   CLASNO, I, J
      CHARACTER CTEMP*8
      INCLUDE 'OBJECT.INC'
      INCLUDE 'VIRTKEYW.INC'
C-----------------------------------------------------------------------
      IERR = 2
C                                       Find class number
      DO 20 I = 1,MAXCLS
         CLASNO = I
         IF (CLASS.EQ.VKWCLS(I)) GO TO 30
 20      CONTINUE
C                                       Didn't find
      MSGTXT = 'OBVHKW: CLASS ' // CLASS // ' NOT DEFINED'
      CALL MSGWRT (7)
      GO TO 999
C                                       Is it already defined?
 30   J = VKWNUM(CLASNO)
      DO 40 I = 1,J
         CALL H2CHR (8, 1, VKWTAH(VKTKEY,I,CLASNO), CTEMP)
         IF (CTEMP.EQ.KEYWRD) GO TO 50
 40      CONTINUE
C                                       Is there room in table: NO
      IF (VKWNUM(CLASNO).GE.MAXVKW) THEN
         MSGTXT = 'OBVHKW: VIRTUAL KEYWORD TABLE FULL FOR CLASS ' //
     *      CLASS
         CALL MSGWRT (7)
         IERR = 1
         GO TO 999
C                                       Add entry to table
      ELSE
         VKWNUM(CLASNO) = VKWNUM(CLASNO) + 1
         I = VKWNUM(CLASNO)
         CALL CHR2H (8, KEYWRD, 1, VKWTAH(VKTKEY,I,CLASNO))
         END IF
C                                       insert new parameters
 50   VKWTAB(VKTCAT,I,CLASNO) = 2
      VKWTAB(VKTPNT,I,CLASNO) = 1
      VKWTAB(VKTYPE,I,CLASNO) = TYPE
      VKWTAB(VKTDM1,I,CLASNO) = 1
      IF (TYPE.EQ.3) VKWTAB(VKTDM1,I,CLASNO) = 8
      VKWTAB(VKTDM2,I,CLASNO) = 1
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE INVINI (IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Initialize virtual keywords for inputs class - none
C   Outputs:
C      IERR   I  Return error code 0=>OK else failed to create class.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LOOP, CLSNUM
      INCLUDE 'OBJECT.INC'
      INCLUDE 'VIRTKEYW.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find next class number
      DO 50 LOOP = 1,MAXCLS
         CLSNUM = LOOP
         IF (VKWCLS(LOOP).EQ.'        ') GO TO 100
 50      CONTINUE
C                                       No more classes available
      IERR = 1
      MSGTXT = 'NO MORE CLASSES CAN BE DEFINED'
      CALL MSGWRT (6)
      GO TO 999
C                                       Initialize to no virtual
C                                       keywords.
 100  VKWNUM(CLSNUM) = 0
C                                       Set class name
      VKWCLS(CLSNUM) = 'INPUTS  '
C
 999  RETURN
      END
      SUBROUTINE IMVINI (IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Initialize virtual keywords for image class.
C   Outputs:
C      IERR   I  Return error code 0=>OK else failed to create class.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER NOKW
C                                       NOKW = No. virtual keywords.
      PARAMETER (NOKW=36)
      INTEGER   LOOP, CLSNUM, NUMVKW, DATCAT(NOKW), DATYPE(NOKW),
     *   DATDM1(NOKW), DATDM2(NOKW)
      CHARACTER DATKEY(NOKW)*8
      INCLUDE 'OBJECT.INC'
      INCLUDE 'VIRTKEYW.INC'
      INCLUDE 'INCS:DHDR.INC'
C                     1           2          3          4
      DATA DATKEY /'OBJECT', 'TELESCOP', 'INSTRUME', 'OBSERVER',
C             5           6         7       8        9       10
     *   'DATE-OBS', 'DATE-MAP', 'BUNIT', 'NDIM', 'NAXIS', 'CTYPE',
C          11       12       13       14       15       16
     *   'CRVAL', 'CDELT', 'CRPIX', 'CROTA', 'EPOCH', 'DATAMAX',
C            17        18        19       20      21     22      23
     *   'DATAMIN', 'PRODUCT', 'NITER', 'BMAJ', 'BMIN', 'BPA', 'VELREF',
C            24        25         26       27          28        29
     *   'ALTRVAL', 'ALTRPIX', 'OBSRA', 'OBSDEC', 'RESTFREQ', 'XSHIFT',
C            30       31         32       33         34        35
     *   'YSHIFT', 'NAMCLSTY', 'IMSEQ', 'USERNO', 'EXTYPE', 'EXTVER',
C          36
     *   'BLANK'/
C                                       Category (1 for catalog stuff)
      DATA DATCAT /NOKW*1/
C                                       Data type 1=D, 2=R, 3=C, 4=I
C                  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
      DATA DATYPE /3, 3, 3, 3, 3, 3, 3, 4, 4, 3, 1, 2, 2, 2, 2, 2, 2, 4,
C       19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
     *   4, 2, 2, 2, 4, 1, 2, 1, 1, 1, 2, 2, 3, 4, 4, 3, 4, 2/
C                                       1st dimension
C                  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
      DATA DATDM1 /8, 8, 8, 8, 8, 8, 8, 1, 7, 8, 7, 7, 7, 7, 1, 1, 1, 1,
C       19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
     *   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,20, 1, 1, 2,20, 1/
C                                       2nd dimension
C                  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
      DATA DATDM2 /1, 1, 1, 1, 1, 1, 1, 1, 1, 7, 1, 1, 1, 1, 1, 1, 1, 1,
C       19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
     *   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,20, 1, 1/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find next class number
      DO 50 LOOP = 1,MAXCLS
         CLSNUM = LOOP
         IF (VKWCLS(LOOP).EQ.'        ') GO TO 100
 50      CONTINUE
C                                       No more classes available
      IERR = 1
      MSGTXT = 'NO MORE CLASSES CAN BE DEFINED'
      CALL MSGWRT (6)
      GO TO 999
C                                       Set class name
 100  VKWCLS(CLSNUM) = 'IMAGE   '
C                                       Initialize virtual keywords.
      NUMVKW = 0
      DO 200 LOOP = 1,NOKW
         NUMVKW = NUMVKW + 1
         CALL CHR2H (8, DATKEY(LOOP), 1, VKWTAH(VKTKEY,NUMVKW,CLSNUM))
         VKWTAB(VKTCAT,NUMVKW,CLSNUM) = DATCAT(LOOP)
         VKWTAB(VKTYPE,NUMVKW,CLSNUM) = DATYPE(LOOP)
         VKWTAB(VKTDM1,NUMVKW,CLSNUM) = DATDM1(LOOP)
         VKWTAB(VKTDM2,NUMVKW,CLSNUM) = DATDM2(LOOP)
 200     CONTINUE
      VKWNUM(CLSNUM) = NUMVKW
C                                       Pointers from include DHDR.INC
      VKWTAB(VKTPNT,1,CLSNUM) = KHOBJ
      VKWTAB(VKTPNT,2,CLSNUM) = KHTEL
      VKWTAB(VKTPNT,3,CLSNUM) = KHINS
      VKWTAB(VKTPNT,4,CLSNUM) = KHOBS
      VKWTAB(VKTPNT,5,CLSNUM) = KHDOB
      VKWTAB(VKTPNT,6,CLSNUM) = KHDMP
      VKWTAB(VKTPNT,7,CLSNUM) = KHBUN
      VKWTAB(VKTPNT,8,CLSNUM) = KIDIM
      VKWTAB(VKTPNT,9,CLSNUM) = KINAX
      VKWTAB(VKTPNT,10,CLSNUM) = KHCTP
      VKWTAB(VKTDM2,10,CLSNUM) = KICTPN
      VKWTAB(VKTPNT,11,CLSNUM) = KDCRV
      VKWTAB(VKTPNT,12,CLSNUM) = KRCIC
      VKWTAB(VKTPNT,13,CLSNUM) = KRCRP
      VKWTAB(VKTPNT,14,CLSNUM) = KRCRT
      VKWTAB(VKTPNT,15,CLSNUM) = KREPO
      VKWTAB(VKTPNT,16,CLSNUM) = KRDMX
      VKWTAB(VKTPNT,17,CLSNUM) = KRDMN
      VKWTAB(VKTPNT,18,CLSNUM) = KITYP
      VKWTAB(VKTPNT,19,CLSNUM) = KINIT
      VKWTAB(VKTPNT,20,CLSNUM) = KRBMJ
      VKWTAB(VKTPNT,21,CLSNUM) = KRBMN
      VKWTAB(VKTPNT,22,CLSNUM) = KRBPA
      VKWTAB(VKTPNT,23,CLSNUM) = KIALT
      VKWTAB(VKTPNT,24,CLSNUM) = KDARV
      VKWTAB(VKTPNT,25,CLSNUM) = KRARP
      VKWTAB(VKTPNT,26,CLSNUM) = KDORA
      VKWTAB(VKTPNT,27,CLSNUM) = KDODE
      VKWTAB(VKTPNT,28,CLSNUM) = KDRST
      VKWTAB(VKTPNT,29,CLSNUM) = KRXSH
      VKWTAB(VKTPNT,30,CLSNUM) = KRYSH
      VKWTAB(VKTPNT,31,CLSNUM) = KHIMN
      VKWTAB(VKTPNT,32,CLSNUM) = KIIMS
      VKWTAB(VKTPNT,33,CLSNUM) = KIIMU
      VKWTAB(VKTPNT,34,CLSNUM) = KHEXT
      VKWTAB(VKTDM2,34,CLSNUM) = KIEXTN
      VKWTAB(VKTPNT,35,CLSNUM) = KIVER
      VKWTAB(VKTDM1,35,CLSNUM) = KIEXTN
      VKWTAB(VKTPNT,36,CLSNUM) = KRBLK
C
 999  RETURN
      END
      SUBROUTINE UVVINI (IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Initialize virtual keywords for UVdata class.
C   Outputs:
C      IERR   I  Return error code 0=>OK else failed to create class.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER NOKW
C                                       NOKW = No. virtual keywords.
      PARAMETER (NOKW=38)
      INTEGER   LOOP, CLSNUM, NUMVKW, DATCAT(NOKW), DATYPE(NOKW),
     *   DATDM1(NOKW), DATDM2(NOKW)
      CHARACTER DATKEY(NOKW)*8
      INCLUDE 'OBJECT.INC'
      INCLUDE 'VIRTKEYW.INC'
      INCLUDE 'INCS:DHDR.INC'
C                     1           2          3          4
      DATA DATKEY /'OBJECT', 'TELESCOP', 'INSTRUME', 'OBSERVER',
C             5           6         7       8        9       10
     *   'DATE-OBS', 'DATE-MAP', 'BUNIT', 'NDIM', 'NAXIS', 'CTYPE',
C          11       12       13       14       15       16
     *   'CRVAL', 'CDELT', 'CRPIX', 'CROTA', 'EPOCH', 'GCOUNT',
C            17        18        19       20        21         22
     *   'NRPARM', 'PTYPE', 'SORTORD', 'VELREF', 'ALTRVAL', 'ALTRPIX',
C            23        24         25       26          27        28
     *   'OBSRA', 'OBSDEC', 'RESTFREQ', 'XSHIFT', 'YSHIFT', 'NAMCLSTY',
C            29       30         31       32      33      34
     *   'IMSEQ', 'USERNO', 'EXTYPE', 'EXTVER', 'LREC', 'NCORR',
C          35
     *   'TYPEUVD', 'BMAJ', 'BMIN', 'BPA'/
C                                       Category (1 for catalog stuff)
      DATA DATCAT /NOKW*1/
C                                       Data type 1=D, 2=R, 3=C, 4=I
C                  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
      DATA DATYPE /3, 3, 3, 3, 3, 3, 3, 4, 4, 3, 1, 2, 2, 2, 2, 4, 4, 3,
C       19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
     *   3, 4, 1, 2, 1, 1, 1, 2, 2, 3, 4, 4, 3, 4, 4, 4, 3, 2, 2, 2/
C                                       1st dimension
C                  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
      DATA DATDM1 /8, 8, 8, 8, 8, 8, 8, 1, 7, 8, 7, 7, 7, 7, 1, 1, 1, 8,
C       19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
     *   2, 1, 1, 1, 1, 1, 1, 1, 1,20, 1, 1, 2,20, 1, 1, 2, 1, 1, 1/
C                                       2nd dimension
C                  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
      DATA DATDM2 /1, 1, 1, 1, 1, 1, 1, 1, 1, 7, 1, 1, 1, 1, 1, 1, 1,
C        18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
     *   14, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find next class number
      DO 50 LOOP = 1,MAXCLS
         CLSNUM = LOOP
         IF (VKWCLS(LOOP).EQ.'        ') GO TO 100
 50      CONTINUE
C                                       No more classes available
      IERR = 1
      MSGTXT = 'NO MORE CLASSES CAN BE DEFINED'
      CALL MSGWRT (6)
      GO TO 999
C                                       Set class name
 100  VKWCLS(CLSNUM) = 'UVDATA   '
C                                       Derived keywords
C                                       LREC
      DATCAT(33) = 3
C                                       NCORR
      DATCAT(34) = 3
C                                       TYPEUVD
      DATCAT(35) = 3
C                                       Initialize virtual keywords.
      NUMVKW = 0
      DO 200 LOOP = 1,NOKW
         NUMVKW = NUMVKW + 1
         CALL CHR2H (8, DATKEY(LOOP), 1, VKWTAH(VKTKEY,NUMVKW,CLSNUM))
         VKWTAB(VKTCAT,NUMVKW,CLSNUM) = DATCAT(LOOP)
         VKWTAB(VKTYPE,NUMVKW,CLSNUM) = DATYPE(LOOP)
         VKWTAB(VKTDM1,NUMVKW,CLSNUM) = DATDM1(LOOP)
         VKWTAB(VKTDM2,NUMVKW,CLSNUM) = DATDM2(LOOP)
 200     CONTINUE
      VKWNUM(CLSNUM) = NUMVKW
C                                       Pointers from include DHDR.INC
      VKWTAB(VKTPNT,1,CLSNUM) = KHOBJ
      VKWTAB(VKTPNT,2,CLSNUM) = KHTEL
      VKWTAB(VKTPNT,3,CLSNUM) = KHINS
      VKWTAB(VKTPNT,4,CLSNUM) = KHOBS
      VKWTAB(VKTPNT,5,CLSNUM) = KHDOB
      VKWTAB(VKTPNT,6,CLSNUM) = KHDMP
      VKWTAB(VKTPNT,7,CLSNUM) = KHBUN
      VKWTAB(VKTPNT,8,CLSNUM) = KIDIM
      VKWTAB(VKTPNT,9,CLSNUM) = KINAX
      VKWTAB(VKTPNT,10,CLSNUM) = KHCTP
      VKWTAB(VKTDM2,10,CLSNUM) = KICTPN
      VKWTAB(VKTPNT,11,CLSNUM) = KDCRV
      VKWTAB(VKTPNT,12,CLSNUM) = KRCIC
      VKWTAB(VKTPNT,13,CLSNUM) = KRCRP
      VKWTAB(VKTPNT,14,CLSNUM) = KRCRT
      VKWTAB(VKTPNT,15,CLSNUM) = KREPO
      VKWTAB(VKTPNT,16,CLSNUM) = KIGCN
      VKWTAB(VKTPNT,17,CLSNUM) = KIPCN
      VKWTAB(VKTPNT,18,CLSNUM) = KHPTP
      VKWTAB(VKTDM2,18,CLSNUM) = KIPTPN
      VKWTAB(VKTPNT,19,CLSNUM) = KITYP
      VKWTAB(VKTPNT,20,CLSNUM) = KIALT
      VKWTAB(VKTPNT,21,CLSNUM) = KDARV
      VKWTAB(VKTPNT,22,CLSNUM) = KRARP
      VKWTAB(VKTPNT,23,CLSNUM) = KDORA
      VKWTAB(VKTPNT,24,CLSNUM) = KDODE
      VKWTAB(VKTPNT,25,CLSNUM) = KDRST
      VKWTAB(VKTPNT,26,CLSNUM) = KRXSH
      VKWTAB(VKTPNT,27,CLSNUM) = KRYSH
      VKWTAB(VKTPNT,28,CLSNUM) = KHIMN
      VKWTAB(VKTPNT,29,CLSNUM) = KIIMS
      VKWTAB(VKTPNT,30,CLSNUM) = KIIMU
      VKWTAB(VKTPNT,31,CLSNUM) = KHEXT
      VKWTAB(VKTDM2,31,CLSNUM) = KIEXTN
      VKWTAB(VKTPNT,32,CLSNUM) = KIVER
      VKWTAB(VKTDM1,32,CLSNUM) = KIEXTN
      VKWTAB(VKTPNT,36,CLSNUM) = KRBMJ
      VKWTAB(VKTPNT,37,CLSNUM) = KRBMN
      VKWTAB(VKTPNT,38,CLSNUM) = KRBPA
C
 999  RETURN
      END
      SUBROUTINE OBCREA (NAME, CLASS, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Associate an object slot with an object name
C   Inputs:
C      NAME    C*32  Name of object
C      CLASS   C*8   Object class
C   Outputs:
C      IERR    I     Error code, 0=OK, 1=None available
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), CLASS*(*)
      INTEGER   IERR
C
      INTEGER   LOOP, OBJNUM, CLSNUM
      CHARACTER TNAME*32, TCLASS*8
      HOLLERITH HNAME(8), HCLASS(2)
      INCLUDE 'OBJECT.INC'
      INCLUDE 'VIRTKEYW.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Create object storage
      TNAME = NAME
      TCLASS = CLASS
      CALL CHR2H (32, TNAME, 1, HNAME)
      CALL CHR2H (8, TCLASS, 1, HCLASS)
      CALL ZOCROB (HNAME, HCLASS, OBJNUM, IERR)
      IF (IERR.EQ.1) THEN
         MSGTXT = 'OBCREA: OBJECT TABLE FULL'
         GO TO 990
         END IF
      IF (IERR.EQ.2) THEN
         MSGTXT = 'OBCREA: MEMORY ALLOCATION FAILED'
         GO TO 990
         END IF
      IF (IERR.NE.0) THEN
         MSGTXT = 'OBCREA: OBJECT CREATION FAILED'
         GO TO 990
         END IF
C                                       Is Class already defined?
      DO 70 LOOP = 1,MAXCLS
         IF (VKWCLS(LOOP).EQ.CLASS) GO TO 999
 70      CONTINUE
C                                       No, find one to assign.
      DO 80 LOOP = 1,MAXCLS
         CLSNUM = LOOP
C                                       Set class name
         IF (VKWCLS(LOOP).EQ.' ') THEN
            VKWCLS(CLSNUM) = CLASS
            GO TO 999
            END IF
 80      CONTINUE
C                                       No more classes available
      IERR = 1
      MSGTXT = 'NO MORE CLASSES CAN BE DEFINED'
      CALL MSGWRT (6)
      MSGTXT = 'PROBLEM WITH OBJECT: ' // NAME
C                                       Error
 990  CALL MSGWRT (6)
      MSGTXT = 'OBCREA: PROBLEM CREATING: ' // NAME
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE OBFREE (NAME, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Frees up object slot associated with name "name" initializing the
C   arrays associated with that slot.
C   Inputs:
C      NAME   C*32  Name of object
C   Outputs:
C      IERR   I     Return error code, 0=OK.
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   OBJNUM
      INCLUDE 'OBJECT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find slot number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Destroy
      CALL ZODEOB (OBJNUM)
C
 999  RETURN
      END
      SUBROUTINE OBNAME (NAME, OBJNUM, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Look up the object slot number of object with name "name".
C   Inputs:
C      NAME   C*32  Name of object
C   Outputs:
C      OBJNUM I     Object slot number
C      IERR   I     Error code, 0=OK, 1=couldn't find
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   OBJNUM, IERR
C
      CHARACTER TNAME*32
      HOLLERITH HNAME(8)
      INCLUDE 'OBJECT.INC'
C-----------------------------------------------------------------------
      IERR = 0
      TNAME = NAME
      CALL CHR2H (32, TNAME, 1, HNAME)
      CALL ZOFNOB (HNAME, OBJNUM)
      IF (OBJNUM.GT.0) GO TO 999
C                                       Not found
      IERR = 1
      OBJNUM = -1
      MSGTXT = 'OBJECT NAMED ' // NAME // ' IS NOT DEFINED'
      CALL MSGWRT (6)
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Look up the class number of object number objnum.
C   Inputs:
C      OBJNUM I     Object slot number
C   Outputs:
C      CLASNO I     Class slot number.
C      CNAME  C*8   Class name
C      IERR   I     Error code, 0=OK, 1=couldn't find
C-----------------------------------------------------------------------
      INTEGER   OBJNUM, CLASNO, IERR
      CHARACTER CNAME*8
C
      INTEGER   LOOP
      HOLLERITH HNAME(8), HCLASS(2)
      CHARACTER TNAME*32
      INCLUDE 'OBJECT.INC'
      INCLUDE 'VIRTKEYW.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get object class name
      TNAME = 'Unknown object'
      CALL ZOFENM (OBJNUM, HNAME, HCLASS, IERR)
      IF (IERR.EQ.4) THEN
         MSGTXT = 'OBCLAS: BAD OBJECT NUMBER'
         GO TO 990
         END IF
      IF (IERR.EQ.5) THEN
         MSGTXT = 'OBCLAS: OBJECT DOES NOT EXIST'
         GO TO 990
         END IF
      IF (IERR.NE.0) THEN
         MSGTXT = 'OBCLAS: ERROR FINDING CLASS'
         GO TO 990
         END IF
      CALL H2CHR (8, 1, HCLASS, CNAME)
      CALL H2CHR (32, 1, HNAME, TNAME)
C                                       Find class in VKWCLS
      DO 50 LOOP = 1,MAXCLS
         CLASNO = LOOP
         IF (VKWCLS(LOOP).EQ.CNAME) GO TO 999
 50      CONTINUE
C                                       Not found
      IERR = 1
      CLASNO = -1
      MSGTXT = 'CLASS NAMED ' // CNAME // ' IS NOT DEFINED'
C                                       Error
 990  CALL MSGWRT (6)
      MSGTXT = 'OBCLAS: PROBLEM WITH: ' // TNAME
      CALL MSGWRT (6)
      WRITE (MSGTXT,1990) OBJNUM
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('OBCLAS: OBJECT NUMBER = ', I5)
      END
      SUBROUTINE OBKEYV (OBJNUM, KEYWRD, KEYPNT, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   See if keyword is an object dependent, virtual keyword.  Called by
C   OBVGET and OBVPUT.
C   Inputs:
C      OBJNUM  I     Object slot number
C      KEYWRD  C*8   Keyword
C   Outputs:
C      KEYPNT  I     Pointer in VKWTAB for virtual keyword.
C      IERR    I     Error code, 0=OK, 1 => not found.
C-----------------------------------------------------------------------
      CHARACTER KEYWRD*(*)
      INTEGER   OBJNUM, KEYPNT, IERR
C
      INTEGER   LOOP, CLASNO
      HOLLERITH TEST(2)
      CHARACTER CNAME*8
      INCLUDE 'OBJECT.INC'
      INCLUDE 'VIRTKEYW.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup class number (CLASNO)
      CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Convert KEYWRD to HOLLERITH
      CALL CHR2H (8, KEYWRD, 1, TEST)
C                                       Find KEYWORD in VKWTAH
      DO 50 LOOP = 1,VKWNUM(CLASNO)
         KEYPNT = LOOP
         IF ((TEST(1).EQ.VKWTAH(1,LOOP,CLASNO))
     *      .AND. (TEST(2).EQ.VKWTAH(2,LOOP,CLASNO))) GO TO 999
 50      CONTINUE
C                                       Not found
      IERR = 1
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE OBPUT (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Save an entry in an object creating it if necessary.
C   Inputs:
C      OBJNUM  I     Object slot number
C      KEYWRD  C*8   Keyword
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of value, an axis dimension of zero
C                    means that that dimension and higher are undefined.
C      VALUE   ?     associated value (non character)
C      VALUEC  C(*)*?   associated value (character)
C   Outputs:
C      IERR    I     Error code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   OBJNUM, TYPE, DIM(*), VALUE(*), IERR
      CHARACTER KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   NVAL, KEYPNT, CLASNO, DISK, CNO, CATPNT, DIM1, DIM2,
     *   NDIM, JERR
      HOLLERITH HTEMP(2), HKEY(2), HNAME(8), HCLASS(2)
      CHARACTER CNAME*8, TNAME*32
      INCLUDE 'OBJECT.INC'
      INCLUDE 'VIRTKEYW.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Size of value
      DIM1 = DIM(1)
      DIM2 = DIM(2)
      NVAL = DIM1 * DIM2
      IF (TYPE.EQ.1) NVAL = NVAL * NWDPDP
C                                       Virtual keyword?
      CALL OBKEYV (OBJNUM, KEYWRD, KEYPNT, IERR)
      IF (IERR.EQ.0) THEN
C                                       Get CATBLK
         CALL ZOFECT (OBJNUM, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
C                                       Find class number
         CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Catalog header?
         IF (VKWTAB(VKTCAT,KEYPNT,CLASNO).EQ.1) THEN
C                                       Check type
            IF (TYPE.NE.VKWTAB(VKTYPE,KEYPNT,CLASNO)) THEN
               IERR = 2
               MSGTXT = 'WRONG KEYWORD DATA TYPE'
               GO TO 990
               END IF
C                                       Check dimensions
            IF ((DIM1.NE.VKWTAB(VKTDM1,KEYPNT,CLASNO)) .OR.
     *         (DIM2.NE.VKWTAB(VKTDM2,KEYPNT,CLASNO))) THEN
               IERR = 2
               MSGTXT = 'WRONG KEYWORD VALUE DIMENSIONALITY'
               GO TO 990
               END IF
C                                       Catalog header pointer
            CATPNT = VKWTAB(VKTPNT,KEYPNT,CLASNO)
C                                       Adjust for Double
            IF (TYPE.EQ.1) CATPNT = (CATPNT-1) * NWDPDP + 1
C                                       Character
            IF (TYPE.EQ.3) THEN
C                                       Check String length
               IF (LEN (VALUEC(1)) .GT. (4*HBFSZ)) THEN
                  IERR = 5
                  MSGTXT = 'OBPUT: EXCESSIVE STRING LENGTH, ARGUMENT'
     *               //  ' ERROR?'
                  GO TO 990
                  END IF
               CALL OBC2H (VALUEC, DIM1, DIM2, CATH(CATPNT))
C                                       Other
            ELSE
               CALL COPY (NVAL, VALUE, CATBLK(CATPNT))
               END IF
C                                       Store CATBLK
            CALL ZOSTCT (OBJNUM, CATBLK, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1001) IERR
               GO TO 990
               END IF
C                                       Catalog keyword?
         ELSE IF (VKWTAB(VKTCAT,KEYPNT,CLASNO).EQ.2) THEN
C                                       Limit on amount of data
            IF (TYPE.EQ.3) NVAL = NVAL / 4
            IF (NVAL.GT.2) THEN
               IERR = 2
               MSGTXT = 'ILLEGAL AMOUNT OF DATA FOR CATALOG KEYWORD.'
               GO TO 990
               END IF
C                                       Write keyword/value in header
            IF (TYPE.NE.3) THEN
C                                       Get DISK and CNO number.
               TNAME = 'Unknown object'
               CALL ZOFENM (OBJNUM, HNAME, HCLASS, JERR)
               IF (JERR.EQ.0) CALL H2CHR (32, 1, HNAME, TNAME)
               CALL OBDSKC (TNAME, DISK, CNO, IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Non-character
               CALL CATKEY ('WRIT', DISK, CNO, KEYWRD, 1, 1, VALUE,
     *            TYPE, SBUFF, IERR)
               IF (IERR.GT.20) IERR = 1
               IF (IERR.NE.0) GO TO 999
            ELSE
C                                       Character
               CALL CHR2H (8, VALUEC, 1, HTEMP)
               CALL CATKEY ('WRIT', DISK, CNO, KEYWRD, 1, 1, HTEMP,
     *            TYPE, SBUFF, IERR)
               IF (IERR.GT.20) IERR = 1
               IF (IERR.NE.0) GO TO 999
               END IF
C                                       Derived keyword?
         ELSE IF (VKWTAB(VKTCAT,KEYPNT,CLASNO).EQ.3) THEN
C                                       Cannot write derived keyword
            IERR = 1
            MSGTXT = 'CANNOT WRITE A DERIVED KEYWORD'
            GO TO 990
C                                       Unknown category
         ELSE
            IERR = 1
            MSGTXT = 'UNKNOWN VIRTUAL KEYWORD CATEGORY'
            GO TO 990
            END IF
         GO TO 999
      ELSE
C                                       Real keyword
         NDIM = 2
         CALL CHR2H (8, KEYWRD, 1, HKEY)
         IF (TYPE.EQ.OOACAR) THEN
C                                       Copy characters to buffer
            IF (4*HBFSZ.LT.DIM1*DIM2) THEN
C                                       Buffer too small
               IERR = 2
               MSGTXT = 'OBPUT: HBUFF TOO SMALL'
               GO TO 990
               END IF
            CALL OBC2H (VALUEC, DIM1, DIM2, HBUFF)
            CALL ZOSTDT (OBJNUM, HKEY, TYPE, NDIM, DIM, HBUFF, IERR)
         ELSE
            CALL ZOSTDT (OBJNUM, HKEY, TYPE, NDIM, DIM, VALUE, IERR)
            END IF
         IF (IERR.EQ.1) THEN
            MSGTXT = 'OBPUT: BAD TYPE'
            GO TO 990
            END IF
         IF (IERR.EQ.2) THEN
            MSGTXT = 'OBPUT: BAD NUMBER OF DIMENSION'
            GO TO 990
            END IF
         IF (IERR.EQ.3) THEN
            MSGTXT = 'OBPUT: BAD DIMENSION ARRAY'
            GO TO 990
            END IF
         IF (IERR.EQ.4) THEN
            MSGTXT = 'OBPUT: BAD OBJECT NUMBER'
            GO TO 990
            END IF
         IF (IERR.EQ.5) THEN
            MSGTXT = 'OBPUT: OBJECT DOES NOT EXIST'
            GO TO 990
            END IF
         IF (IERR.EQ.6) THEN
            MSGTXT = 'OBPUT: ERROR ALLOCATING KEYWORD STORAGE'
            GO TO 990
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
      MSGTXT = 'KEYWORD =' // KEYWRD
      CALL MSGWRT (6)
      TNAME = 'Unknown object'
      CALL ZOFENM (OBJNUM, HNAME, HCLASS, JERR)
      IF (JERR.EQ.0) CALL H2CHR (32, 1, HNAME, TNAME)
      MSGTXT = 'OBPUT: PROBLEM WITH OBJECT: ' // TNAME
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OBPUT: ERROR ', I5,' COPYING CATBLK')
 1001 FORMAT ('OBPUT: ERROR ', I5,' SAVING CATBLK')
      END
      SUBROUTINE OBRGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Fetch the value (array) for a specified real (non-virtual) keyword.
C   Inputs:
C      OBJNUM  I     Object slot number
C      KEYWRD  C*8   Keyword
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of value, an axis dimension of zero
C                    means that that dimension and higher are undefined.
C      VALUE   ?     associated value (non character)
C      VALUEC  C(*)*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   OBJNUM, TYPE, DIM(*), VALUE(*), IERR
      CHARACTER KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   DIM1, DIM2, OTYPE, ONDIM, ODIM(5), JERR
      CHARACTER TNAME*32
      HOLLERITH HKEY(2), HNAME(8), HCLASS(2)
      INCLUDE 'OBJECT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find type, check existance
      CALL CHR2H (8, KEYWRD, 1, HKEY)
      CALL ZOINLE (OBJNUM, HKEY, OTYPE, ONDIM, ODIM, IERR)
      IF (IERR.EQ.1) THEN
         MSGTXT = 'OBRGET: KEYWORD ' // KEYWRD // ' NOT FOUND'
         GO TO 990
         END IF
      IF (IERR.EQ.4) THEN
         MSGTXT = 'OBRGET: BAD OBJECT NUMBER'
         GO TO 990
         END IF
      IF (IERR.EQ.5) THEN
         MSGTXT = 'OBRGET: OBJECT DOES NOT EXIST'
         GO TO 990
         END IF
      DIM1 = MAX (1, ODIM(1))
      DIM2 = MAX (1, ODIM(2))
      IF (OTYPE.EQ.OOACAR) THEN
C                                       Copy characters to buffer
         IF (4*HBFSZ.LT.DIM1*DIM2) THEN
C                                       Buffer too small
            IERR = 2
            MSGTXT = 'OBPUT: HBUFF TOO SMALL'
            GO TO 990
            END IF
         CALL ZOFEDT (OBJNUM, HKEY, TYPE, ONDIM, DIM, HBUFF, IERR)
         CALL OBH2C (HBUFF, DIM1, DIM2, VALUEC)
      ELSE
         CALL ZOFEDT (OBJNUM, HKEY, TYPE, ONDIM, DIM, VALUE, IERR)
         END IF
C                                       Errors should have been already
C                                       caught.
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
      MSGTXT = 'KEYWORD =' // KEYWRD
      CALL MSGWRT (6)
      TNAME = 'Unknown object'
      CALL ZOFENM (OBJNUM, HNAME, HCLASS, JERR)
      IF (JERR.EQ.0) CALL H2CHR (32, 1, HNAME, TNAME)
      MSGTXT = 'OBRGET: PROBLEM WITH OBJECT: ' // TNAME
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE OBGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Fetch the value (array) for a specified keyword.
C   Inputs:
C      OBJNUM  I     Object slot number
C      KEYWRD C*8   Keyword
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of value, an axis dimension of zero
C                    means that that dimension and higher are undefined.
C      VALUE   ?     associated value (non character)
C      VALUEC  C(*)*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   OBJNUM, TYPE, DIM(*), VALUE(*), IERR
      CHARACTER KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   NVAL, KEYPNT, CLASNO, DISK, CNO, CATPNT, DIM1, DIM2, IT,
     *   LOOP , I1, I2, JERR
      HOLLERITH HTEMP(2), HNAME(8), HCLASS(2)
      CHARACTER CNAME*8, TNAME*32, CTYPE*4, UVTYPS(8)*2, CTYPE2*4
      INCLUDE 'OBJECT.INC'
      INCLUDE 'VIRTKEYW.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C                                       ??, vis, Single dish, projected
C                                       celestial, galactic, ecliptic,
C                                       switched beam
      DATA UVTYPS /'??', 'UV', 'SD', 'SC', 'SG', 'SE', 'SB', 'UP'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Virtual keyword?
      CALL OBKEYV (OBJNUM, KEYWRD, KEYPNT, IERR)
      IF (IERR.EQ.0) THEN
C                                       Find class number
         CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Get keyword type dimension
         TYPE = VKWTAB(VKTYPE,KEYPNT,CLASNO)
         DIM1 = VKWTAB(VKTDM1,KEYPNT,CLASNO)
         DIM2 = VKWTAB(VKTDM2,KEYPNT,CLASNO)
C                                       Size of value
         NVAL = DIM1 * DIM2
         IF (TYPE.EQ.1) NVAL = NVAL * NWDPDP
C                                       Get CATBLK
         CALL ZOFECT (OBJNUM, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
C                                       Catalog header?
         IF (VKWTAB(VKTCAT,KEYPNT,CLASNO).EQ.1) THEN
C                                       Catalog header pointer
            CATPNT = VKWTAB(VKTPNT,KEYPNT,CLASNO)
C                                       Adjust for Double
            IF (TYPE.EQ.1) CATPNT = (CATPNT-1) * NWDPDP + 1
C                                       Get data: Character
            IF (TYPE.EQ.3) THEN
C                                       Check String length
               IF (LEN (VALUEC(1)) .GT. (4*HBFSZ)) THEN
                  IERR = 5
                  MSGTXT = 'OBGET: EXCESSIVE STRING LENGTH, ARGUMENT' //
     *               ' ERROR?'
                  GO TO 990
                  END IF
               CALL OBH2C (CATH(CATPNT), DIM1, DIM2, VALUEC)
C                                       Other
            ELSE
               CALL COPY (NVAL, CATBLK(CATPNT), VALUE)
               END IF
C                                       Catalog keyword?
         ELSE IF (VKWTAB(VKTCAT,KEYPNT,CLASNO).EQ.2) THEN
C                                       Get DISK and CNO number.
            TNAME = 'Unknown object'
            CALL ZOFENM (OBJNUM, HNAME, HCLASS, JERR)
            IF (JERR.EQ.0) CALL H2CHR (32, 1, HNAME, TNAME)
            CALL OBDSKC (TNAME, DISK, CNO, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Read keyword/value in header
            DIM1 = 1
            DIM2 = 1
            I1 = 1
            CALL CATKEY ('READ', DISK, CNO, KEYWRD, I1, I2, HTEMP,
     *         TYPE, SBUFF, IERR)
            IF (IERR.GT.20) IERR = 1
            IF (IERR.NE.0) GO TO 999
            IF (TYPE.EQ.3) THEN
C                                       Check String length
               IF (LEN (VALUEC(1)) .GT. (4*HBFSZ)) THEN
                  IERR = 5
                  MSGTXT = 'OBGET: EXCESSIVE STRING LENGTH, ARGUMENT' //
     *               ' ERROR?'
                  GO TO 990
                  END IF
C                                       Character
               DIM1 = 8
               CALL H2CHR (DIM1, 1, HTEMP, VALUEC)
            ELSE
C                                       Non-character
               NVAL = 1
C                                       Double?
               IF (TYPE.EQ.1) NVAL = NWDPDP
               CALL COPY (NVAL, HTEMP, VALUE)
               END IF
C                                       Derived keyword?
         ELSE IF (VKWTAB(VKTCAT,KEYPNT,CLASNO).EQ.3) THEN
            DIM(1) = 1
            DIM(2) = 1
            DIM(3) = 0
C                                       LREC (uv data)
            IF (KEYWRD.EQ.'LREC') THEN
               IT = CATBLK(KINAX)
               DO 500 LOOP = 2,CATBLK(KIDIM)
                  IF (CATBLK(KINAX+LOOP-1).GT.0)
     *               IT = IT * CATBLK(KINAX+LOOP-1)
 500              CONTINUE
               VALUE(1) = IT + CATBLK(KIPCN)
               TYPE = 4
C                                       NCORR (uv data)
            ELSE IF (KEYWRD.EQ.'NCORR') THEN
               IT = 1
               DO 510 LOOP = 2,CATBLK(KIDIM)
                  IF (CATBLK(KINAX+LOOP-1).GT.0)
     *               IT = IT * CATBLK(KINAX+LOOP-1)
 510              CONTINUE
               VALUE(1) = IT
               TYPE = 4
C                                       TYPEUVD (uv data)
            ELSE IF (KEYWRD.EQ.'TYPEUVD') THEN
               I1 = 1
               I2 = 1
               DIM(1) = 2
               DO 520 LOOP = 1,CATBLK(KIPCN)
                  IT = (LOOP - 1) * 2 + KHPTP
                  CALL H2CHR (4, 1, CATH(IT), CTYPE)
                  CALL H2CHR (4, 5, CATH(IT), CTYPE2)
                  IF (CTYPE.EQ.'UU-L') THEN
                     I1 = 2
                     IF (CTYPE2.EQ.'-NCP') I1 = 8
                     END IF
                  IF (CTYPE.EQ.'VV-L') THEN
                     I2 = 2
                     IF (CTYPE2.EQ.'-NCP') I2 = 8
                     END IF
                  IF (CTYPE.EQ.'RA  ') I1 = 3
                  IF (CTYPE.EQ.'DEC ') I2 = 3
                  IF (CTYPE.EQ.'RA--') I1 = 4
                  IF (CTYPE.EQ.'DEC-') I2 = 4
                  IF (CTYPE.EQ.'GLON') THEN
                     I1 = 3
                     IF (CTYPE2(:1).EQ.'-') I1 = 5
                     END IF
                  IF (CTYPE.EQ.'GLAT') THEN
                     I2 = 3
                     IF (CTYPE2(:1).EQ.'-') I2 = 5
                     END IF
                  IF (CTYPE.EQ.'ELON') THEN
                     I1 = 3
                     IF (CTYPE(:1).EQ.'-') I1 = 6
                     END IF
                  IF (CTYPE.EQ.'ELAT') THEN
                     I2 = 3
                     IF (CTYPE(:1).EQ.'-') I2 = 6
                     END IF
                  IF (CTYPE.EQ.'AZ--') I1 = 7
                  IF (CTYPE.EQ.'EL--') I2 = 7
 520              CONTINUE
               IF (I1.NE.I2) I1 = 1
               VALUEC(1) = UVTYPS(I1)
               TYPE = 3
C                                       Unknown
            ELSE
               IERR = 1
               MSGTXT = 'UNKNOWN DERIVED KEYWORD: ' // KEYWRD
               GO TO 990
               END IF
C                                       Unknown category
         ELSE
            IERR = 1
            MSGTXT = 'UNKNOWN VIRTUAL KEYWORD CATEGORY'
            GO TO 990
            END IF
C                                       Save dimensionality
         DIM(1) = DIM1
         DIM(2) = DIM2
         DIM(3) = 0
         GO TO 999
C                                       Try real keyword.
      ELSE
         CALL OBRGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
 995  MSGTXT = 'KEYWORD =' // KEYWRD
      CALL MSGWRT (6)
      TNAME = 'Unknown object'
      CALL ZOFENM (OBJNUM, HNAME, HCLASS, JERR)
      IF (JERR.EQ.0) CALL H2CHR (32, 1, HNAME, TNAME)
      MSGTXT = 'OBGET: PROBLEM WITH OBJECT: ' // TNAME
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OBGET: ERROR ', I5,' COPYING CATBLK')
      END
      SUBROUTINE OBLUN (LUN, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Find a free LUN. Maintains an internal list and then checks to see
C   if it's used by AIPS.
C   Outputs:
C      LUN   I  Returned LUN to use
C      IERR  I  Error code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   LUN, IERR
C
      INTEGER  LOOP, FIND, NUMCHK, LULIST(20), JERR, JERR2, NUMTOT
      INCLUDE 'OBJECT.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                       NUMCHK = no. LUNs to check
      DATA NUMCHK /20/
      DATA NUMTOT /80/
C                                       LULIST = potential LUN list
      DATA LULIST /16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 40, 41, 42,
     *   43, 44, 45, 46, 47, 48, 49/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Loop over possible LUNs
      DO 20 LOOP = 1,NUMTOT
         IF (LOOP.LE.NUMCHK) THEN
            LUN = LULIST(LOOP)
         ELSE
            LUN = LOOP - NUMCHK + 60
            END IF
C                                       Check internal list
         IF ((.NOT.LUNUSE(LUN)) .AND. (DEVTAB(LUN).EQ.0)) THEN
C                                       Use LSERCH to see if LUN in use.
            CALL LSERCH ('SRCH', LUN, FIND, .TRUE., JERR)
C                                       "non map" use
            CALL LSERCH ('SRCH', LUN, FIND, .FALSE., JERR2)
C                                       Not in use?
            IF ((JERR.EQ.1) .AND. (JERR2.EQ.1)) GO TO 900
            END IF
 20      CONTINUE
C                                       No Free LUNs on list.
      MSGTXT = 'NO MORE AIPS LUNS AVAILABLE'
      CALL MSGWRT (7)
C                                       Count usage
      IERR = 0
      DO 30 LOOP = 1,NUMTOT
         IF (LOOP.LE.NUMCHK) THEN
            LUN = LULIST(LOOP)
         ELSE
            LUN = LOOP - NUMCHK + 60
            END IF
C                                       Check internal list
         IF ((LUNUSE(LUN)) .AND. (DEVTAB(LUN).EQ.0)) THEN
C                                       Use LSERCH to see if LUN in use.
            CALL LSERCH ('SRCH', LUN, FIND, .TRUE., JERR)
C                                       "non map" use
            CALL LSERCH ('SRCH', LUN, FIND, .FALSE., JERR2)
C                                       Not in use?
            IF ((JERR.EQ.1) .AND. (JERR2.EQ.1)) IERR = IERR + 1
            END IF
 30      CONTINUE
      WRITE (MSGTXT,1030) IERR, NUMTOT
      CALL MSGWRT (7)
      IERR = 1

      GO TO 999
C                                       Mark this LUN as assigned
 900  LUNUSE(LUN) = .TRUE.
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('OBLUN:',I3,' OF',I3,' POSSIBLE LUNS ACTUALLY OPEN NOW')
      END
      SUBROUTINE OBLUFR (LUN)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Free an LUN from the list of used LUNs.
C   Inputs:
C      LUN   I  Returned LUN to use
C-----------------------------------------------------------------------
      INTEGER   LUN
C
      INCLUDE 'OBJECT.INC'
C-----------------------------------------------------------------------
      LUNUSE(LUN) = .FALSE.
C
 999  RETURN
      END
      SUBROUTINE OBINFO (NAME, BUFNO, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Look up I/O stream associated with an object.
C   Inputs:
C      NAME   C*?   Name of object
C   Outputs:
C      BUFNO  I     Object buffer number
C      IERR   I     Error code, 0=OK. 1= file not open
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   BUFNO, IERR
C
      INTEGER   I
      INCLUDE 'OBJECT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Look up in active buffer table
      BUFNO = -1
      DO 100 I = 1,MAXIO
         IF (NAME.EQ.BUFNAM(I)) THEN
            BUFNO = I
            GO TO 999
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE OBDSKC (NAME, DISK, CNO, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Return Disk and slot information for object.
C   Inputs:
C      NAME   C*32  Name of object
C   Outputs:
C      DISK   I     Disk number
C      CNO    I     Catalog slot number
C      IERR   I     Error code, 0=OK. 1= file not open
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   DISK, CNO, IERR
C
      INTEGER   OBJNUM, IT1, DIM(7), DUMMY(1)
      CHARACTER CDUMMY*1
      INCLUDE 'OBJECT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find object number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find disk  and slot
      CALL OBRGET (OBJNUM, 'DISK', IT1, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DISK = DUMMY(1)
      CALL OBRGET (OBJNUM, 'CNO', IT1, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CNO = DUMMY(1)
C
 999  RETURN
      END
      SUBROUTINE OBHGET (NAME, CAT, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Return catalog header record for an object.
C   Inputs:
C      NAME   C*32   Name of object
C   Outputs:
C      CAT    I(256) Catalog header record
C      IERR   I      Error code, 0=OK. 1= file not open
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   CAT(256), IERR
C
      INTEGER   OBJNUM
      INCLUDE 'OBJECT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find object number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy header
      CALL ZOFECT (OBJNUM, CAT, IERR)
C
 999  RETURN
      END
      SUBROUTINE OBHPUT (NAME, CAT, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Stores catalog header record for an object.
C   Inputs:
C      NAME   C*32   Name of object
C      CAT    I(256) Catalog header record
C   Outputs:
C      IERR   I      Error code, 0=OK. 1= file not open
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   CAT(256), IERR
C
      INTEGER   OBJNUM
      INCLUDE 'OBJECT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find object number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy header
      CALL ZOSTCT (OBJNUM, CAT, IERR)
C
 999  RETURN
      END
      SUBROUTINE OBCOPY (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Copies one onject to another.
C   Inputs:
C      NAMEIN C*32   Name of input object
C      NAMOUT C*(*)   Name of output object
C   Outputs:
C      IERR   I      Error code, 0=OK. 1= file not open
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*(*), NAMOUT*(*)
      INTEGER   IERR
C
      HOLLERITH HNAMEI(8), HNAMEO(8)
      INCLUDE 'OBJECT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Copy
      CALL CHR2H (32, NAMEIN, 1, HNAMEI)
      CALL CHR2H (32, NAMOUT, 1, HNAMEO)
      CALL ZOCPOB (HNAMEI, HNAMEO, IERR)
      IF (IERR.EQ.1) THEN
         MSGTXT = 'OBCOPY: PROBLEM WITH INPUT OBJECT'
         GO TO 990
         END IF
      IF (IERR.EQ.2) THEN
         MSGTXT = 'OBCOPY: PROBLEM CREATING NEW OBJECT'
         GO TO 990
         END IF
      IF (IERR.EQ.3) THEN
         MSGTXT = 'OBCOPY: PROBLEM WITH COPYING LINKED LIST'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
      MSGTXT = 'OBCOPY: ERROR COPYING ' // NAMEIN
      CALL MSGWRT (6)
      MSGTXT = '     TO ' // NAMOUT
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE OBOPEN (NAME, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   Assigns a buffer
C   Inputs:
C      NAME   C*32  Name of object
C   Outputs:
C      IERR   I     Error code, 0=OK.
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   LOOP, BUFNO
      INCLUDE 'OBJECT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Does it already have one
C                                       assigned?
      CALL OBINFO (NAME, BUFNO, IERR)
      IERR = 0
C                                       If Buffer/I/O stream not
C                                       assigned then assign one.
      IF (BUFNO.LE.0) THEN
         DO 100 LOOP = 1,MAXIO
            BUFNO = LOOP
            IF (.NOT.OBNUSE(LOOP)) GO TO 110
 100        CONTINUE
C                                       No more I/O streams = buffers
C                                       available.
         IERR = 2
         MSGTXT = 'NO MORE BUFFERS AVAILABLE'
         CALL MSGWRT (6)
         GO TO 999
C                                       Assign buffer
 110     BUFNAM(BUFNO) = NAME
         OBNUSE(BUFNO) = .TRUE.
C                                       Set buffer pointer
         BUFPNT(BUFNO) = 1
         END IF
C
 999  RETURN
      END
      SUBROUTINE OBCLOS (NAME, IERR)
C-----------------------------------------------------------------------
C   Private to Object manager and Class I/O functions
C   I/O Buffer is deassigned.
C   Inputs:
C      NAME   C*32  Name of object
C   Outputs:
C      IERR   I     Error code, 0=OK. 1= file not open
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   BUFNO
      INCLUDE 'OBJECT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find object number
      CALL OBINFO (NAME, BUFNO, IERR)
      IERR = 0
      IF ((BUFNO.LE.0) .OR. (BUFNO.GT.MAXIO)) GO TO 999
C                                       Deallocate buffer
      OBNUSE(BUFNO) = .FALSE.
      BUFNAM(BUFNO) = '    '
C                                       Reset buffer pointer
      BUFPNT(BUFNO) = -1
C
 999  RETURN
      END
      SUBROUTINE OBC2H (STRING, DIM1, DIM2, HOLL)
C-----------------------------------------------------------------------
C   Private to Object manager.
C   Convert a string (possibly an array) to hollerith.
C   This routine should deal with either the case that the input string
C   is declared either as C*(DIM1*DIM2) or C(DIM1)*DIM2.
C   Inputs:
C      STRING   C(*)*?  A string, possibly an array.
C      DIM1     I       First dimension of character array
C      DIM2     I       Second dimension of character array
C   Outputs:
C      HOLL     H(*)    Hollerith version of character string.
C      IERR     I       Error code, 0=OK. 1= file not open
C-----------------------------------------------------------------------
      CHARACTER STRING(*)*(*)
      INTEGER   DIM1, DIM2
      HOLLERITH HOLL(*)
C
      INTEGER   SLEN, LOOP, D1, D2, NVAL, INDEX
C-----------------------------------------------------------------------
      D1 = MAX (1, DIM1)
      D2 = MAX (1, DIM2)
C                                       Check string length
      SLEN = LEN (STRING(1))
      IF (SLEN .GE. (D1*D2)) THEN
C                                       STRING passed as one string
         NVAL = D1 * D2
         CALL CHR2H (NVAL, STRING, 1, HOLL)
      ELSE
C                                       STRING passed as array.
         INDEX = 1
         DO 100 LOOP = 1,D2
            CALL CHR2H (D1, STRING(LOOP), INDEX, HOLL)
            INDEX = INDEX + D1
 100        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE OBH2C (HOLL, DIM1, DIM2, STRING)
C-----------------------------------------------------------------------
C   Private to Object manager.
C   Convert a hollerith to a string (possibly an array).
C   This routine should deal with either the case that the output string
C   is declared either as C*(DIM1*DIM2) or C(DIM1)*DIM2.
C   Inputs:
C      HOLL     H(*)    Hollerith version of character string.
C      DIM1     I       First dimension of character array
C      DIM2     I       Second dimension of character array
C   Outputs:
C      STRING   C(*)*?  A string, possibly an array.
C      IERR     I       Error code, 0=OK. 1= file not open
C-----------------------------------------------------------------------
      CHARACTER STRING(*)*(*)
      INTEGER   DIM1, DIM2
      HOLLERITH HOLL(*)
C
      INTEGER   SLEN, LOOP, D1, D2, NVAL, INDEX
C-----------------------------------------------------------------------
      D1 = MAX (1, DIM1)
      D2 = MAX (1, DIM2)
C                                       Check string length
      SLEN = LEN (STRING(1))
      IF (SLEN .GE. (D1*D2)) THEN
C                                       STRING passed as one string
         NVAL = D1 * D2
         CALL H2CHR (NVAL, 1, HOLL, STRING)
      ELSE
C                                       STRING passed as array.
         INDEX = 1
         DO 100 LOOP = 1,D2
            CALL H2CHR (D1, INDEX, HOLL, STRING(LOOP))
            INDEX = INDEX + D1
 100        CONTINUE
         END IF
C
 999  RETURN
      END


