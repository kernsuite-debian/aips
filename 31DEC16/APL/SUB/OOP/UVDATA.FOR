C    UVdata Class Module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "UVdata" class library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2011-2012, 2015
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
C    UV data generally implies data measured with an interferometer.
C    A UV data object consists of an set of arrays of correlation values
C    with a set of "random parameters" as labels for each set of
C    correlation values.  Also included are a set of descriptive
C    classes.  General access to members is through member functions
C    OUVGET and OUVPUT.  Efficient access to visibility array data
C    is through UVREAD and UVWRIT.
C    Class public members:
C      VIS         R(*)          Random parameters and correlation
C                                array.
C      FILE_NAME   Base class    File name information
C      FILE_STATUS Base class    File status information
C      UV_DESC     Base class    Descriptive information about the uv
C                                data set
C      VELOCITY    Base class    Information for the conversion of
C                                frequency to velocity.
C      POSITION    Base class    Celestial position information
C      CALEDIT     Base class    Calibration and editing control
C                                information.
C      OLDFILE     Logical       If true then the underlying files
C                                existed before the object was created.
C
C   Class  private data:
C      UVNRPM    I(MAXIO)   Number of random parameters
C      UVLREC    I(MAXIO)   Length of visibility record.
C      UVNVIS    I(MAXIO)   Number of visibility records.
C      UVBIND    I(MAXIO)   Buffer pointer.
C      UVSIZE    I(MAXIO)   Maximum file size in vis records.
C      UVVNO     I(MAXIO)   Last visibility number to be written.
C      UVWSPT    I(MAXIO)   Index of WEIGHT and SCALE random parameters
C                           for compressed data.
C      UVRAW     L(MAXIO)   Is file open for "raw" reading
C      UVNVB     I(MAXIO)   Number of visibilities in buffer (raw read)
C      UVNXVB    I(MAXIO)   Next visibility to return from buffer (raw
C                            read)
C      UVMVRD    I(MAXIO)   Max number of visibilities to read in one
C                            call to UVDISK (raw read)
C      UVSCAL    R(MAXIO)   Scaling applied to u,v,w on read: keyword
C                            DOUVSCAL=true allows this to correct for
C                            frequency changes made by UVGET
C
C   Public functions:
C     OUVCRE (name, ierr)
C        Creates an uv data set object with name "name".  Initializes
C        sel/cal/edit criteria.
C     OUVDES (name, ierr)
C        Destroys the uv data set object with name "name";
C        quasi-permanent  forms are unaffected.
C     OUVZAP (name, ierr)
C        Destroys the uv data set object with name "name";
C        quasi-permanent forms are deleted.
C     OUVCOP (namein, namout, ierr)
C        Copys one object to another.  The same quasi permanent forms
C        are used for both.
C     OUVCLN (namein, namout, ierr)
C        CLONES an object.  A new object is created and any associated
C        quasi-permanent forms are created.  The name, class etc. for
C        the output quasi-permanent catalog entries are given by
C        keywords OUTNAME, OUTCLASS, OUTSEQ and OUTDISK associated with
C        namein.
C     OUVSCR (name, name2, nvis, scrcmp, ierr)
C        Creates an uv data set scratch object of the structure given by
C        name2 and number of visibilities given by nvis.
C     OUVOPN (name, status, ierr)
C        Opens an uv data set object.  Checks for valid data.
C        Initializes I/O, selection, calibration, and editing for read.
C     OUVCLO (name, ierr)
C        Closes an uv data set object.  Updates data validity.
C     OUVINI (name, ierr)
C        Reinitialize I/O.
C     OUVGET (name, keywrd, type, dim, value, valuec, ierr)
C        Return keyword value.
C     OUVPUT (name, keywrd, type, dim, value, valuec, ierr)
C        Store keyword value.
C     UVREAD (name, rp, vis, ierr)
C        Read next visibility record.  The UV_DESC descriptor for an
C        open file will describe the data as returned rather than as on
C        disk.  Will open file if necessary.
C     UVWRIT (name, rp, vis, ierr)
C        Write next visibility record.  Will open file if necessary.
C   Following are public but shouldn't be needed from applications.
C     OUVATT (name, docrea, ierr)
C        Attach an AIPS catalog data file to an object.  The name, class
C        etc. for the output quasi-permanent catalog entries are given
C        by keywords NAME, CLASS, SEQ and DISK associated with NAME.
C        Creates the file if necessary.
C     OUCSET (name, status, ierr)
C        Sets any file status)
C     OUCCLR (name, ierr)
C        Clears any file status except 'DEST'.
C     OUCDES (name, status, ierr)
C        Clears destroy on fail status and resets file status.  If
C        status is blank no status is set.
C     OUVSLT (name, name2, ierr)
C        Modify descriptors in name to correspond to selection criteria
C        in name2.
C     OUVCGT (name, cat, ierr)
C        Return catalog header record for an uvdata indicating correct
C        compression. This function should be used to get the CATBLK to
C        be passed to nonOOP routines.
C
C   Shared with derived classes
C
C   Private functions:
C     OUCFST (name, disk, cno, status, ierr)
C        Set AIPS catalog status, DFIL.INC common
C     OUCFCL (name, disk, cno, status, ierr)
C        Clear AIPS catalog status, DFIL.INC common
C     OUCREA (name, ierr)
C        Creates file structures for uv data set "name"
C     OUBGET (name, keywrd, type, dim, value, valuec, ierr)
C        Fetches member of a base class of uv data set class
C     OUBPUT (name, keywrd, type, dim, value, valuec, ierr)
C        Stores member of a base class of uv data set class
C     OUVXPN (name, nvadd, ierr)
C        Expands size of uv data file
C     OUVCPR (name, ierr)
C        Compresses size of uv data file
C     OUVTNF (catin, catout)
C       Copies table information from CATBLK catin to catout.
C     OUVPAK (name, pack, ierr)
C        Set or reset axis dimensions and random parameters for packed
C        uv data.
C-----------------------------------------------------------------------
LOCAL INCLUDE 'UVDATA.INC'
C                                       UVdata class I/O
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   UVNRPM(MAXIO), UVLREC(MAXIO), UVNVIS(MAXIO),
     *   UVBIND(MAXIO), UVSIZE(MAXIO), UVVNO(MAXIO), UVWSPT(MAXIO),
     *   UVNVB(MAXIO), UVNXVB(MAXIO), UVMVRD(MAXIO), UVLOCU(MAXIO)
      REAL      UVSCAL(MAXIO)
      LOGICAL   SINGLE(MAXIO), UVRAW(MAXIO)
      COMMON /UVCNUM/ UVNRPM, UVLREC, UVNVIS, UVBIND, UVSIZE, UVVNO,
     *   UVWSPT, UVNVB, UVNXVB, UVMVRD, SINGLE, UVRAW, UVSCAL, UVLOCU
LOCAL END
      SUBROUTINE OUVCRE (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates an uv data set object with name "name".
C   Simple return if object exists.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
C
      INTEGER   IDIM(7), VISOFF, OBJNUM, MSGSAV, DUMMY(1)
      CHARACTER STATUS*4, CDUMMY*1
      LOGICAL   LDUMMY(1)
      EQUIVALENCE (LDUMMY, DUMMY)
      INCLUDE 'INCS:PAOOF.INC'
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
      CALL OBCREA (NAME, 'UVDATA  ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Initilize sel/cal/edit
      CALL SECINI (NAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       File status init blank
      STATUS = ' '
      IDIM(1) = 4
      IDIM(2) = 1
      IDIM(3) = 0
      CALL FSTPUT (NAME, 'STATUS', OOACAR, IDIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Default is valid.
      IDIM(1) = 1
      CALL FSTPUT (NAME, 'VALID', OOALOG, IDIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       VISOFF init. to 0
      VISOFF = 0
      CALL UVDPUT (NAME, 'VISOFF', OOAINT, IDIM, VISOFF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Default = old file
      IDIM(1) = 1
      LDUMMY(1) = .TRUE.
      CALL OUVPUT (NAME, 'OLDFILE', OOALOG, IDIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'OUVCRE: ERROR CREATING ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OUVDES (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the uv data set object with name "name"; quasi-permanent
C   forms are unaffected.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL OBFREE (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE OUVZAP (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the uv data set object with name "name"; quasi-permanent
C   forms are deleted.
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
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find disk  and slot
      CALL OBDSKC (NAME, DISK, CNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get catalog header
      CALL OBHGET (NAME, CAT, IERR)
      IF (IERR.NE.0) GO TO 995
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
      GO TO 999
C                                       Error
 995  MSGTXT = 'OUVZAP: ERROR DESTROYING ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OUVOPN (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Opens a uv data set file.  Obtains header info etc.
C   For uv data sets to be written the validity of the data is checked.
C   If an object is opened write and VISOFF is larger than the current
C   size of the object then it is expanded.
C   Inputs:
C      NAME   C*?   The name of the object.
C      STATUS C*4   'READ', 'WRIT', 'DEST' (write but destroy on
C                     failure), or 'RRAW' (read but do not apply
C                     calibration or editing)
C   Output:
C      IERR  I     Error return code, 0=OK, 5=data invalid
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   TYPE, IDIM(7), OBJNUM, BUFNO, VO, BO, NPIO, BUFSZ, DISK,
     *   CNO, NV, ITEMP, NVADD, ILOCF, DUMMY(1), MSGSAV, I,
     *   ISFOFF(2*MAXIF)
      REAL      RP, VIS, CRPIXI(7)
      DOUBLE PRECISION CRVAL(7), INFREQ, SFOFF(MAXIF)
      CHARACTER OPCO*4, FNAM*48, CDUMMY*1, UVTYPE*2
      LOGICAL   VALID, DOCRE, ISCOMP, DOSCAL, LDUMMY(1)
      EQUIVALENCE (LDUMMY, DUMMY), (ISFOFF, SFOFF)
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'UVDATA.INC'
C-----------------------------------------------------------------------
      IERR = 0
      DOSCAL = .FALSE.
      CALL DFILL (MAXIF, 0.0D0, SFOFF)
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Attach to file
      DOCRE = (STATUS .NE. 'READ') .AND. (STATUS .NE. 'RRAW')
      CALL OUVATT (NAME, DOCRE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       AIPS status
      CALL OUCSET (NAME, STATUS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       For Read checks that file valid
      IF ((STATUS.EQ.'READ') .OR. (STATUS.EQ.'RRAW')) THEN
         CALL FSTGET (NAME, 'VALID', TYPE, IDIM, VALID, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         IF (.NOT.VALID) THEN
            MSGTXT = NAME // ' UV DATA INVALID'
            IERR = 5
            GO TO 990
            END IF
C                                       Setup cal/edit/sel
         IF (STATUS.EQ.'READ') THEN
            CALL SECSET (NAME, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
C                                       scaling?
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL OUVGET (NAME, 'DOUVSCAL', TYPE, IDIM, DUMMY, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.NE.0) THEN
            LDUMMY(1) = .FALSE.
            IERR = 0
            END IF
         DOSCAL = LDUMMY(1)
         END IF
C                                       For Write, mark as invalid.
      IF (STATUS.EQ.'WRIT') THEN
         VALID = .FALSE.
         IDIM(1) = 1
         IDIM(2) = 1
         IDIM(3) = 0
         CALL FSTPUT (NAME, 'VALID', OOALOG, IDIM, VALID, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Save reference frequency
      CALL UVDFND (NAME, 2, 'FREQ', ILOCF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR FINDING INPUT FREQUENCY AXIS'
         CALL MSGWRT (7)
         GO TO 995
         END IF
      CALL UVDGET (NAME, 'CRVAL', TYPE, IDIM, CRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IDIM(1) = 1
      IDIM(2) = 1
      INFREQ = CRVAL(ILOCF)
      CALL UVDPUT (NAME, 'REFFREQ ', OOADP, IDIM, INFREQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (NAME, 'CRPIX', TYPE, IDIM, CRPIXI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IDIM(1) = 1
      IDIM(2) = 1
      CALL UVDPUT (NAME, 'REFFPIX ', OOARE, IDIM, CRPIXI(ILOCF), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Set up for I/O
C                                       Open object (assign buffer)
      CALL OBOPEN (NAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get buffer no.
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get VISOFF
      CALL UVDGET (NAME, 'VISOFF', TYPE, IDIM, VO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get VISOFF
      CALL UVDGET (NAME, 'TYPEUVD', TYPE, IDIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 995
      SINGLE(BUFNO) = UVTYPE(1:1).EQ.'S'
C                                       For 'READ' this is all done in
C                                       UVGET.
      IF (STATUS.EQ.'READ') THEN
C                                       Find name etc
         CALL FNAGET (NAME, 'NAME', TYPE, IDIM, DUMMY, UNAME, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL FNAGET (NAME, 'CLASS', TYPE, IDIM, DUMMY, UCLAS, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL FNAGET (NAME, 'DISK', TYPE, IDIM, ITEMP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         UDISK = ITEMP
         CALL FNAGET (NAME, 'IMSEQ', TYPE, IDIM, ITEMP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         USEQ = ITEMP
C                                       Open/init
         INITVS = VO
         UVSCAL(BUFNO) = 1.0
         IF (SINGLE(BUFNO)) THEN
            CALL SDGET ('INIT', RP, VIS, IERR)
         ELSE
            CALL UVGET ('INIT', RP, VIS, IERR)
            IF ((DOSCAL) .AND. (IERR.EQ.0)) UVSCAL(BUFNO) = FREQ/UVFREQ
            UVLOCU(BUFNO) = ILOCU + 1
            CALL DPCOPY (MAXIF, SFREQS, SFOFF)
            END IF
         IF (IERR.GT.0) GO TO 995
         IF (IERR.LT.0) THEN
            IF (SINGLE(BUFNO)) THEN
               CALL SDGET ('CLOS', RP, VIS, I)
            ELSE
               CALL UVGET ('CLOS', RP, VIS, I)
               END IF
            MSGTXT = 'NO DATA SELECTED'
            GO TO 990
            END IF
C                                       Reset next vis.
         UVVNO(BUFNO) = VO
C                                       Save table info.  Table info is
C                                       on CATUV in DSEL.INC but not on
C                                       CATBLK from UVGET.
         CALL OUVTNF (CATUV, CATBLK, IERR)
         IF (IERR.GT.0) GO TO 995
C                                       Save UVGET's CATBLK
         CALL OBHPUT (NAME, CATBLK, IERR)
         IF (IERR.GT.0) GO TO 995
C
         UVRAW(BUFNO) = .FALSE.
         IDIM(1) = MAXIF
         IDIM(2) = 1
C                                       source freq offset
         CALL OUVPUT (NAME, 'SOURFREQ', OOADP, IDIM, ISFOFF, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
C                                       Assign LUN
         CALL OBLUN (OBJLUN(BUFNO), IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Get sizes: NVIS
         CALL OBGET (OBJNUM, 'GCOUNT', TYPE, IDIM, UVNVIS(BUFNO),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       No. random parameters.
         CALL OBGET (OBJNUM, 'NRPARM', TYPE, IDIM, UVNRPM(BUFNO),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Record length
         CALL OBGET (OBJNUM, 'LREC', TYPE, IDIM, UVLREC(BUFNO), CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Find disk and slot
         CALL OBDSKC (NAME, DISK, CNO, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       File name
         CALL UVDGET (NAME, 'FNAME', TYPE, IDIM, DUMMY, FNAM, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Open UV file
         CALL ZOPEN (OBJLUN(BUFNO), OBJFIN(BUFNO), DISK, FNAM, .TRUE.,
     *      .FALSE., .TRUE., IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
C                                       If the file is compressed then
C                                       cache the location of the WEIGHT
C                                       and SCALE parameters for use in
C                                       UVWRIT otherwise set
C                                       UVWSPT(BUFNO) to -1 to indicate
C                                       that the data is not compressed.
         CALL UVDGET (NAME, 'ISCOMP', TYPE, IDIM, ISCOMP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         IF (ISCOMP) THEN
            CALL UVDFND (NAME, 1, 'WEIGHT', UVWSPT(BUFNO), IERR)
            IF (IERR.EQ.1) IERR = 0
            IF (IERR.NE.0) GO TO 995
         ELSE
            UVWSPT(BUFNO) = -1
            END IF
C                                       Get file size in vis
         CALL ZEXIST (DISK, FNAM, UVSIZE(BUFNO), IERR)
         IERR = 0
         UVSIZE(BUFNO) = (UVSIZE(BUFNO) * 256.0D0) / UVLREC(BUFNO)
C                                       Need to expand file?
         IF (VO.GT.UVNVIS(BUFNO)) THEN
            NVADD = (VO - UVNVIS(BUFNO)) + 20
            CALL OUVXPN (NAME, NVADD, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
C                                       Init I/O
         IF (STATUS .EQ. 'RRAW') THEN
            OPCO = 'READ'
         ELSE
            OPCO = 'WRIT'
            END IF
         BO = 1
         UVVNO(BUFNO) = VO
         NPIO = 1
         BUFSZ = BUFSIZ * 2
         IF (OPCO .EQ. 'WRIT') THEN
C                                       Allow writing past current EOF
            NV = 1000000000
            NPIO = 1
         ELSE
            NV = UVNVIS(BUFNO)
            NPIO = 0
            END IF
         CALL UVINIT (OPCO, OBJLUN(BUFNO), OBJFIN(BUFNO), NV, VO,
     *      UVLREC(BUFNO), NPIO, BUFSZ, OBUFFR(1,BUFNO), BO,
     *      UVBIND(BUFNO), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1001) IERR
            GO TO 990
            END IF
         IF (STATUS .EQ. 'RRAW') THEN
            UVRAW(BUFNO) = .TRUE.
            UVNVB(BUFNO) = 0
C                                       Nothing read yet
            UVNXVB(BUFNO) = 1
C                                       Force physical read on next call
C                                       to UVREAD
            UVMVRD(BUFNO) = NPIO
         ELSE
            UVRAW(BUFNO) = .FALSE.
            END IF
C                                       no scaling on write, read raw
         UVSCAL(BUFNO) = 1.0
C                                       no source freq offset
         IDIM(1) = MAXIF
         IDIM(2) = 1
         CALL OUVPUT (NAME, 'SOURFREQ', OOADP, IDIM, ISFOFF, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
      GO TO 999
C                                       Error
  990 CALL MSGWRT (7)
  995 MSGTXT = 'OUVOPN: ERROR OPENING ' // NAME
      CALL MSGWRT (7)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUVOPN: ERROR ',I3,' OPENING UV DATA')
 1001 FORMAT ('OUVOPN: ERROR ',I3,' INITIALIZING UV DATA')
      END
      SUBROUTINE OUVCLO (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Closes uv data set updating disk resident information.  For files
C   being written, the validity of the data is set.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   BUFNO, CAT(256), DISK, CNO, TYPE, DIM(7), GCOUNT, NIOUT,
     *   DUMMY(1)
      REAL      RP, VIS
      CHARACTER STATUS*4, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'UVDATA.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get buffer no.
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Lookup STATUS
      CALL FSTGET (NAME, 'STATUS', TYPE, DIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 995
      IF ((STATUS.EQ.'WRIT') .OR. (STATUS.EQ.'DEST')) THEN
C                                       Update GCOUNT
         CALL UVDGET (NAME, 'GCOUNT', TYPE, DIM, GCOUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Can't really stop writing in the
C                                       middle of a file.
         GCOUNT = UVVNO(BUFNO)
         CALL UVDPUT (NAME, 'GCOUNT', OOAINT, DIM, GCOUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Make sure that the header
C                                       reflects whether or not the UV
C                                       data is packed in case it has
C                                       been changed since the file was
C                                       opened (this may not help if
C                                       the PTYPE names have been
C                                       changed).
         CALL OUVPAK (NAME, (UVWSPT(BUFNO).GT.0), IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Get CATBLK
         CALL OBHGET (NAME, CAT, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Update AIPS catalog header for
C                                       write
C                                       Find disk and slot
         CALL OBDSKC (NAME, DISK, CNO, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Store  CATBLK
         CALL CATIO ('UPDT', DISK, CNO, CAT, 'REST', SBUFF, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            GO TO 995
            END IF
         IERR = 0
C                                       Mark as valid.
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL FSTPUT (NAME, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       AIPS status
      CALL OUCCLR (NAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Close as necessary
      IF (STATUS.EQ.'READ') THEN
         IF (UVRAW(BUFNO)) THEN
            CALL ZCLOSE (OBJLUN(BUFNO), OBJFIN(BUFNO), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1002) IERR
               GO TO 990
               END IF
            CALL OBLUFR (OBJLUN(BUFNO))
            UVRAW(BUFNO) = .FALSE.
         ELSE
C                                       Read use UVGET
            IF (SINGLE(BUFNO)) THEN
               CALL SDGET ('CLOS', RP, VIS, IERR)
            ELSE
               CALL UVGET ('CLOS', RP, VIS, IERR)
               END IF
            IF (IERR.GT.0) GO TO 995
            END IF
      ELSE
C                                       Write - flush buffer and close.
         NIOUT = 0
         CALL UVDISK ('FLSH', OBJLUN(BUFNO), OBJFIN(BUFNO),
     *      OBUFFR(1,BUFNO), NIOUT, UVBIND(BUFNO), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1001) IERR
            GO TO 990
            END IF
C                                       Compress if necessary
         CALL OUVCPR (NAME, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Close AIPS I/O
         CALL ZCLOSE (OBJLUN(BUFNO), OBJFIN(BUFNO), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1002) IERR
            GO TO 990
            END IF
C                                       Release LUN
         CALL OBLUFR (OBJLUN(BUFNO))
         END IF
C                                       Close object
      CALL OBCLOS (NAME, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
  990 CALL MSGWRT (7)
  995 MSGTXT = 'OUVCLO: ERROR CLOSING ' // NAME
      CALL MSGWRT (7)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUVCLO: CATIO ERROR ',I3,' UPDATING CATALOG')
 1001 FORMAT ('OUVCLO: ERROR ',I3,' FLUSHING I/O')
 1002 FORMAT ('OUVCLO: ERROR ',I3,' CLOSING FILE')
      END
      SUBROUTINE OUVINI (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Reinitialize I/O.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   BUFNO, TYPE, IDIM(7), GCOUNT, NV, VO, NIOUT, BO,
     *   NPIO, BUFSZ, NVADD, DUMMY(1)
      REAL      RP, VIS
      CHARACTER STATUS*4, OPCO*4, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'UVDATA.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get buffer no.
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get status
      CALL FSTGET (NAME, 'STATUS', TYPE, IDIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close as necessary
      IF (STATUS.EQ.'READ') THEN
C                                       Read use UVGET, may already be
C                                       closed.
         IF (BUFNO.GT.0) THEN
            IF (SINGLE(BUFNO)) THEN
               CALL SDGET ('CLOS', RP, VIS, IERR)
            ELSE
               CALL UVGET ('CLOS', RP, VIS, IERR)
               END IF
            IF (IERR.GT.0) GO TO 995
            END IF
      ELSE
C                                       Write - flush buffer
         NIOUT = 0
         CALL UVDISK ('FLSH', OBJLUN(BUFNO), OBJFIN(BUFNO),
     *      OBUFFR(1,BUFNO), NIOUT, UVBIND(BUFNO), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
C                                       Update GCOUNT
         CALL UVDGET (NAME, 'GCOUNT', TYPE, IDIM, GCOUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         GCOUNT = UVVNO(BUFNO)
         CALL UVDPUT (NAME, 'GCOUNT', OOAINT, IDIM, GCOUNT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Re initialize
C                                       Get VISOFF
      CALL UVDGET (NAME, 'VISOFF', TYPE, IDIM, VO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Read use UVGET
      IF (STATUS.EQ.'READ') THEN
         INITVS = VO
         IF (SINGLE(BUFNO)) THEN
            CALL SDGET ('INIT', RP, VIS, IERR)
         ELSE
            CALL UVGET ('INIT', RP, VIS, IERR)
            END IF
         IF (IERR.GT.0) GO TO 995
C                                       Reset next vis.
         UVVNO(BUFNO) = VO
      ELSE
C                                       Init I/O
         OPCO = STATUS
         IF (OPCO.EQ.'DEST') OPCO = 'WRIT'
         BO = 1
         UVVNO(BUFNO) = VO
         NPIO = 1
         BUFSZ = BUFSIZ * 2
         NV = UVNVIS(BUFNO)
C                                       Need to expand file?
         IF (VO.GT.NV) THEN
            NVADD = (VO - NV) + 20
            CALL OUVXPN (NAME, NVADD, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
C                                       Allow writing past current EOF
         IF (OPCO.EQ.'WRIT') NV = 1000000000
         CALL UVINIT (OPCO, OBJLUN(BUFNO), OBJFIN(BUFNO), NV, VO,
     *      UVLREC(BUFNO), NPIO, BUFSZ, OBUFFR(1,BUFNO), BO,
     *      UVBIND(BUFNO), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1001) IERR
            GO TO 990
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'ERROR REINITIALIZING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I3,' FLUSHING BUFFER')
 1001 FORMAT ('ERROR ',I3,' INITIALIZING UV DATA')
      END
      SUBROUTINE OUVGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Returns the dimensionality and value(s) associated with a given
C   keyword.
C   Note: for KEYWRD='VIS' VALUE is tha rp array and VALUEC is the
C   visibility array.
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
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Is this a base class?
      CALL OUBGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
      IF (IERR.EQ.1) CALL OBGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE,
     *   VALUEC, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE OUVPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Stores the value (array) associated with a given keyword.
C   Note: for KEYWRD='VIS' VALUE is tha rp array and VALUEC is the
C   visibility array.
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
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Is this a base class?
      CALL OUBPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
      IF (IERR.EQ.1) CALL OBPUT (OBJNUM, KEYWRD, TYPE, DIM, VALUE,
     *   VALUEC, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE UVREAD (NAME, RP, VIS, IERR)
C-----------------------------------------------------------------------
C   Public function
C    Read next visibility record.  The UV_DESC descriptor for an
C    open file will describe the data as returned rather than as on
C    disk.  Will open the file if necessary
C   Inputs:
C      NAME     C*?   The name of the object.
C   Outputs:
C      RP       R(*)  Random parameter array.
C      VIS      R(*)  Visibility data array.
C      IERR     I     Error return code, 0=OK, -1 => end of data
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      REAL      RP(*), VIS(*)
      INTEGER   IERR
C
      INTEGER   BUFNO, NPIO, POINT, NCOPY
      INCLUDE 'UVDATA.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get buffer no.
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open if necessary
      IF (BUFNO.LT.0) THEN
         CALL OUVOPN (NAME, 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OBINFO (NAME, BUFNO, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Read next vis
      IF (UVRAW(BUFNO)) THEN
         IF (UVNXVB(BUFNO) .GT. UVNVB(BUFNO)) THEN
C                                       need another buffer load
            NPIO = UVMVRD(BUFNO)
            CALL UVDISK ('READ', OBJLUN(BUFNO), OBJFIN(BUFNO),
     *                   OBUFFR(1, BUFNO), NPIO, UVBIND(BUFNO), IERR)
C                                       check for EOF before errors
            IF ((IERR.EQ.4) .OR. (NPIO.EQ.0)) THEN
               IERR = -1
            ELSE IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR
               GO TO 990
               END IF
            UVNVB(BUFNO) = NPIO
            UVNXVB(BUFNO) = 1
            END IF
         IF (IERR.EQ.0) THEN
C                                       not EOF
            POINT = UVBIND(BUFNO) + (UVNXVB(BUFNO) - 1) * UVLREC(BUFNO)
            CALL RCOPY (UVNRPM(BUFNO), OBUFFR(POINT, BUFNO), RP)
            POINT = POINT + UVNRPM(BUFNO)
            NCOPY = UVLREC(BUFNO) - UVNRPM(BUFNO)
            IF (UVWSPT(BUFNO) .GT. 0) THEN
               CALL ZUVXPN (NCOPY, OBUFFR(POINT, BUFNO),
     *            RP(UVWSPT(BUFNO)), VIS)
            ELSE
               CALL RCOPY (NCOPY, OBUFFR(POINT, BUFNO), VIS)
               END IF
            UVNXVB(BUFNO) = UVNXVB(BUFNO) + 1
            UVVNO(BUFNO)  = UVVNO(BUFNO) + 1
            END IF
      ELSE

         IF (SINGLE(BUFNO)) THEN
            CALL SDGET ('READ', RP, VIS, IERR)
         ELSE
            CALL UVGET ('READ', RP, VIS, IERR)
            IF ((IERR.EQ.0) .AND. (UVSCAL(BUFNO).NE.0.0) .AND.
     *         (UVSCAL(BUFNO).NE.1.0)) THEN
               RP(UVLOCU(BUFNO)) = RP(UVLOCU(BUFNO)) * UVSCAL(BUFNO)
               RP(UVLOCU(BUFNO)+1) = RP(UVLOCU(BUFNO)+1) * UVSCAL(BUFNO)
               RP(UVLOCU(BUFNO)+2) = RP(UVLOCU(BUFNO)+2) * UVSCAL(BUFNO)
               END IF
            END IF
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
C                                       Update next vis.
         UVVNO(BUFNO) = UVVNO(BUFNO) + 1
         END IF
      GO TO 999
C                                       Error
  990 CALL MSGWRT (7)
  995 MSGTXT = 'UVREAD: ERROR READING ' // NAME
      CALL MSGWRT (7)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVREAD: ERROR ',I3,' READING UV DATA')
      END
      SUBROUTINE UVWRIT (NAME, RP, VIS, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Writes next visibility record.    Will open the file if necessary.
C   Inputs:
C      NAME     C*?   The name of the object.
C      RP       R(*)  Random parameter array.
C      VIS      R(*)  Visibility data array.
C   Outputs:
C      IERR     I     Error return code, 0=OK, -1 => end of data
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      REAL      RP(*), VIS(*)
      INTEGER   IERR
C
      INTEGER   BUFNO, NIOUT, POINT, I, NCOPY
      INCLUDE 'UVDATA.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get buffer no.
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open if necessary
      IF (BUFNO.LT.0) THEN
         CALL OUVOPN (NAME, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Copy to output buffer
      POINT = UVBIND(BUFNO)
      DO 10 I = 1,UVNRPM(BUFNO)
         OBUFFR(POINT,BUFNO) = RP(I)
         POINT = POINT + 1
   10 CONTINUE
      NCOPY = UVLREC(BUFNO) - UVNRPM(BUFNO)
      IF (UVWSPT(BUFNO).GT.0) THEN
         CALL ZUVPAK (NCOPY, VIS,
     *      OBUFFR(UVBIND(BUFNO) + UVWSPT(BUFNO) - 1, BUFNO),
     *      OBUFFR(POINT, BUFNO))
      ELSE
         DO 20 I = 1,NCOPY
            OBUFFR(POINT,BUFNO) = VIS(I)
            POINT = POINT + 1
   20       CONTINUE
         END IF
C                                       Need to Expand?
      IF (UVVNO(BUFNO).GE.UVSIZE(BUFNO)) CALL OUVXPN (NAME, 1000, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Write
      NIOUT = 1
      CALL UVDISK ('WRIT', OBJLUN(BUFNO), OBJFIN(BUFNO),
     *   OBUFFR(1,BUFNO), NIOUT, UVBIND(BUFNO), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Update next vis.
      UVVNO(BUFNO) = UVVNO(BUFNO) + 1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'UVWRIT: ERROR WRITING ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVWRIT: ERROR ',I3,' WRITING UV DATA')
      END
      SUBROUTINE OUVCOP (NAMEIN, NAMOUT, IERR)
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
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL OBCOPY (NAMEIN, NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OUVATT (NAMOUT, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'OUVCOP: ERROR COPYING ' // NAMEIN
      CALL MSGWRT (7)
      MSGTXT = 'OUVCOP: TO ' // NAMOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OUVCLN (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Clones an object.  A new object is created and any associated
C   quasi-permanent forms are created.  The name, class etc. for the
C   output quasi-permanent catalog entries are given by keywords
C   OUTNAME, OUTCLASS, OUTSEQ and OUTDISK associated with namein.  The
C   output uv data set will represent the specified subuv data set in
C   the input uv data set.  If the DOUVCOMP keyword is set on NAMEIN
C   and has the value .TRUE. then the quasi-permanent data structure
C   associated with NAMOUT will be compressed if there is more than
C   one correlator channel selected in NAMEIN.
C      The CALEDIT member is initialized in the clone object.
C   Inputs:
C      NAMEIN  C*?   The name of the input object.
C      NAMOUT  C*?   The name of the output object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*(*), NAMOUT*(*)
      INTEGER   IERR
C
      INTEGER  SEQO, SEQI, DISKO, DISKI, CNOI, DIM(7), TYPE, BUFNO,
     *   DUMMY(1), MSGSAV, NFRQAV
      CHARACTER NAMO*12, CLAO*6, NAMI*12, CLAI*6, DEFCLS*6, CHTEMP*20,
     *   CDUMMY*1
      LOGICAL   DOOPEN, DOCOMP, LDUMMY(1)
      EQUIVALENCE (DUMMY, LDUMMY)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      IERR = 0
C                                       See if input open (has an
C                                       assigned buffer)
      CALL OBINFO (NAMEIN, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
      DOOPEN = BUFNO.LE.0
C                                       Open input if necessary
      IF (DOOPEN) THEN
         CALL OUVOPN (NAMEIN, 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Copy object
      CALL OBCOPY (NAMEIN, NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Make new files - get CATBLK
      CALL OBHGET (NAMEIN, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Find old disk and slot
      CALL OBDSKC (NAMEIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get new name etc info.
      CALL OUVGET (NAMEIN, 'OUTNAME ', TYPE, DIM, DUMMY, NAMO, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OUVGET (NAMEIN, 'OUTCLASS', TYPE, DIM, DUMMY, CLAO, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OUVGET (NAMEIN, 'OUTSEQ  ', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      SEQO = DUMMY(1)
      CALL FNAGET (NAMEIN, 'NAMCLSTY', TYPE, DIM, DUMMY, CHTEMP, IERR)
      IF (IERR.NE.0) GO TO 995
      NAMI = CHTEMP(1:12)
      CLAI = CHTEMP(13:18)
      CALL FNAGET (NAMEIN, 'IMSEQ', TYPE, DIM, SEQI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       frequency averaging
      MSGSUP = 32000
      CALL OUVGET (NAMEIN, 'AVERAGEF', TYPE, DIM, DUMMY, CDUMMY, IERR)
      MSGSUP = MSGSAV
      NFRQAV = DUMMY(1)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NFRQAV = 1
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (NFRQAV.GT.1) THEN
         CALL UVPGET (IERR)
         CATBLK(KINAX+JLOCF) = CATBLK(KINAX+JLOCF) / NFRQAV
         CATR(KRCRP+JLOCF) = (CATR(KRCRP+JLOCF) - 1.0 -
     *      ((NFRQAV-1.0)/2.0)) / NFRQAV + 1
         CATR(KRCIC+JLOCF) = CATR(KRCIC+JLOCF) * NFRQAV
         END IF
C                                       Close input if opened in this
C                                       routine.
      IF (DOOPEN) THEN
         CALL OUVCLO (NAMEIN, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Put info in CATBLK
      DEFCLS = '      '
      CALL MAKOUT (NAMI, CLAI, SEQI, DEFCLS, NAMO, CLAO, SEQO)
      CALL CHR2H (12, NAMO, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAO, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQO
C                                       Save header
      CALL OBHPUT (NAMOUT, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Set disk number
      CALL OUVGET (NAMEIN, 'OUTDISK ', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      DISKO = DUMMY(1)
      CALL OUVPUT (NAMOUT, 'DISK', OOAINT, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Set compression state
      MSGSUP = 32000
      CALL OUVGET (NAMEIN, 'DOUVCOMP', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         LDUMMY(1) = .FALSE.
         IERR = 0
         END IF
      DOCOMP = LDUMMY(1)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 995
      TYPE = OOALOG
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL UVDPUT (NAMOUT, 'ISCOMP', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create output file.
      CALL OUCREA (NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Initialise CALEDIT member
      CALL SECINI (NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error.
 995  MSGTXT = 'OUVCLN: ERROR CLONING ' // NAMEIN
      CALL MSGWRT (7)
      MSGTXT = 'OUVCLN: TO ' // NAMOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OUVSCR (NAME, NAME2, NVIS, CMPSCR, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates an uv data set scratch object of the structure given by
C   name2 and number of visibilities given by nvis.
C   Selection by frequency, IF and polarization in NAME2 is supported.
C   Adds keyword 'SCRCNO' to NAME giving the CFIL.INC scratch file
C   number which may be neded by AIPS utility routines.
C      The disk-resident data for the scratch file will be compressed
C   if CMPSCR is true and more than one correlator is selected in
C   NAME2.
C      If the object is already fully instantiated nothing is done.
C   Inputs:
C      NAME    C*?   The name of the output object.
C      NAME2   C*?   The name of the object defining the structure
C      NVIS    I     The number of visibility records needed, 0 => 1000
C      CMPSCR  L     Compress scratch data?
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), NAME2*(*)
      INTEGER   NVIS, IERR
      LOGICAL   CMPSCR
C
      INTEGER  SIZE, LREC, TYPE, IDIM(7), CAT(256), NVT, VO, GC, USID,
     *   SEQ, DUMMY(1), NCORR, NRPARM, WTIDX
      CHARACTER FTYPE*8, STATUS*4, NAMO*12, CLAO*6, TYPO*2, STAT*4,
     *   CDUMMY*1
      LOGICAL   EXIST
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
      NVT = NVIS
C                                       Does it already exist
      CALL OBFEXS (NAME, EXIST, IERR)
      IERR = 0
      IF (EXIST) GO TO 999
C                                       Check input
      IF (NVIS.LE.0) NVT = 1000
C                                       Get LREC
      CALL UVDGET (NAME2, 'LREC', TYPE, IDIM, LREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (LREC.LE.0) THEN
         MSGTXT = 'ATTEMPT TO DEFINE SCRATCH FILE ZERO LREC'
         IERR = 2
         GO TO 990
         END IF
C                                       Adjust LREC if the output data
C                                       will be compressed.
      CALL UVDGET (NAME2, 'NCORR', TYPE, IDIM, NCORR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (CMPSCR.AND.(NCORR.GT.1)) THEN
         CALL UVDGET (NAME2, 'NRPARM', TYPE, IDIM, NRPARM, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         CALL UVDFND (NAME2, 1, 'WEIGHT', WTIDX, IERR)
         IF (IERR.EQ.1) IERR = 0
         IF (IERR.NE.0) GO TO 995
         IF (WTIDX.LE.0) THEN
C                                       Need to add WEIGHT/SCALE
            NRPARM = NRPARM + 2
            END IF
         LREC = NRPARM + NCORR
         END IF
C                                       Create object
      CALL OUVCRE (NAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Size of scratch file, add some
C                                       cushion.
      SIZE = ((LREC * NVT) + 2048) / 256
C                                       Create Scratch file.
      CALL SCREAT (SIZE, SBUFF, IERR)
C                                       Register in DFIL.INC
      IF ((IERR.EQ.0) .AND. (NCFILE.LT.FILIST)) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = SCRVOL(NSCR)
         FCNO(NCFILE) = SCRCNO(NSCR)
         FRW(NCFILE) = 2
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Fetch  CATBLK
      CALL CATIO ('READ', SCRVOL(NSCR), SCRCNO(NSCR), CAT, 'REST',
     *   SBUFF, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      IERR = 0
C                                       Hate CATDIR
      CALL CATDIR ('INFO', SCRVOL(NSCR), SCRCNO(NSCR), NAMO, CLAO, SEQ,
     *   TYPO, USID, STAT, SBUFF, IERR)
      IF (((IERR.GE.1) .AND. (IERR.LE.4)) .OR. (IERR.EQ.6)) THEN
         WRITE (MSGTXT,1001) IERR
         GO TO 990
         END IF
      IERR = 0
C                                       Save name info or CATDIR will
C                                       screw you to the wall (again).
      CALL CHR2H (12, NAMO, KHIMNO, CAT(KHIMN))
      CALL CHR2H (6, CLAO, KHIMCO, CAT(KHIMC))
      CALL CHR2H (2, TYPO, KHPTYO, CAT(KHPTY))
      CAT(KIIMS) = SEQ
C                                       Save header
      CALL OBHPUT (NAME, CAT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Copy structure
      CALL UVDCOP (NAME2, NAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Set compression flag and update
C                                       header.
      IDIM(1) = 1
      IDIM(2) = 1
      IDIM(3) = 0
      IF (CMPSCR.AND.(NCORR.GT.1)) THEN
         CALL OUVPAK (NAME, .TRUE., IERR)
         IF (IERR.NE.0) GO TO 995
         CALL UVDPUT (NAME, 'ISCOMP', OOALOG, IDIM, .TRUE., CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         CALL OUVPAK (NAME, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 995
         CALL UVDPUT (NAME, 'ISCOMP', OOALOG, IDIM, .FALSE., CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Update size (mark as empty)
      IDIM(1) = 1
      IDIM(2) = 1
      IDIM(3) = 0
      GC = 0
      CALL UVDPUT (NAME, 'GCOUNT', OOAINT, IDIM, GC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Reset VISOFF
      VO = 0
      CALL UVDPUT (NAME, 'VISOFF', OOAINT, IDIM, VO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Selection by channel, IF, stokes
C                                       ALREADY DONE IN OPENING NAME2 AND
C                                       CALLING UVDCOP
C      CALL OUVSLT (NAME, NAME2, IERR)
C      IF (IERR.NE.0) GO TO 995
C                                       Save Name info
      CALL FNAPUT (NAME, 'DISK', OOAINT, IDIM, SCRVOL(NSCR), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FNAPUT (NAME, 'CNO', OOAINT, IDIM, SCRCNO(NSCR), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
      IDIM(1) = 12
      CALL FNAPUT (NAME, 'NAME', OOACAR, IDIM, DUMMY, NAMO, IERR)
      IF (IERR.NE.0) GO TO 999
      IDIM(1) = 6
      CALL FNAPUT (NAME, 'CLASS', OOACAR, IDIM, DUMMY, CLAO, IERR)
      IF (IERR.NE.0) GO TO 999
      IDIM(1) = 1
      CALL FNAPUT (NAME, 'IMSEQ', OOAINT, IDIM, SEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       CFIL.INC scratch file number.
      DUMMY(1) = NSCR
      CALL OUVPUT (NAME, 'SCRCNO', OOAINT, IDIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      FTYPE = 'AIPS'
      IDIM(1) = 8
      CALL FNAPUT (NAME, 'FTYPE', OOACAR, IDIM, DUMMY, FTYPE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       File status init 'WRIT'
      STATUS = 'WRIT'
      IDIM(1) = 4
      CALL FSTPUT (NAME, 'STATUS', OOACAR, IDIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Mark as invalid.
      IDIM(1) = 1
      CALL FSTPUT (NAME, 'VALID', OOALOG, IDIM, .FALSE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Store Modified header to disk
C                                       Fetch  CATBLK
      CALL OBHGET (NAME, CAT, IERR)
      IF (IERR.NE.0) GO TO 995
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
 995  MSGTXT = 'OUVSCR: ERROR MAKING SCRATCH UV  ' // NAME
      CALL MSGWRT (7)
      MSGTXT = 'OUVSCR: FROM UV  ' // NAME2
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' READING SCRATCH FILE CATBLK')
 1001 FORMAT ('ERROR',I3,' LOOKING UP SCRATCH FILE')
      END
      SUBROUTINE OUVATT (NAME, DOCREA, IERR)
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
      INTEGER   CAT(256), SEQI, DISKI, CNOI, DIM(7), TYPE, EIF, ECHAN,
     *   NAXIS(7), INDX, DUMMY(1), BIF, BCHAN
      CHARACTER NAMI*12, CLAI*6, PTYPE*2, STAT*4, FNAME*48, FTYPE*8,
     *   ANAME*8, NAMCL*20, CDUMMY*1
      LOGICAL   ISCOMP
      HOLLERITH CATH(256)
      EQUIVALENCE (CAT, CATH)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IERR = 0
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
         NAMCL = NAMI // CLAI // 'UV'
         TYPE = 3
         DIM(1) = 20
         DIM(2) = 1
         CALL FNAPUT (NAME, 'NAMCLSTY', OOACAR, DIM, DUMMY, NAMCL, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Create
         CALL OUCREA (NAME, IERR)
         IF (IERR.NE.0) GO TO 999
         CNOI = 1
C                                       Try again.
         CALL CATDIR ('SRCH', DISKI, CNOI, NAMI, CLAI, SEQI, PTYPE,
     *      NLUSER, STAT, SBUFF, IERR)
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, NAMI, CLAI, SEQI, DISKI, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKI, CNOI, CAT, 'REST', SBUFF, IERR)
C                                       Damn catalog flags
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1001) IERR
         GO TO 990
         END IF
      IERR = 0
C                                       Save header
      CALL OBHPUT (NAME, CAT, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
C                                       Save file information
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
      CALL UVDPUT (NAME, 'FNAME', OOACAR, DIM, DUMMY, FNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      CALL UVDPUT (NAME, 'FDISK', OOAINT, DIM, DISKI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       ANAME
      ANAME = '  '
      DIM(1) = 8
      CALL UVDPUT (NAME, 'ANAME', OOACAR, DIM, DUMMY, ANAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Compressed on disk?
      ISCOMP = CAT(KINAX) .EQ. 1
      DIM(1) = 1
      CALL UVDPUT (NAME, 'ISCOMP', OOALOG, DIM, ISCOMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default IFs
      CALL UVDGET (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDFND (NAME, 2, 'IF  ', INDX, IERR)
      IF ((INDX.GT.0) .AND. (IERR.EQ.0)) THEN
         CALL SECGET (NAME, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL SECGET (NAME, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         BIF = MAX (1, BIF)
         IF (EIF.LT.BIF) EIF = NAXIS(INDX)
         IF (BIF.GT.NAXIS(INDX)) BIF = 1
         EIF = MIN (EIF, NAXIS(INDX))
      ELSE
         EIF = 1
         BIF = 1
         END IF
      DIM(1) = 1
      CALL SECPUT (NAME, 'BIF', OOAINT, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL SECPUT (NAME, 'EIF', OOAINT, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default ECHAN
      CALL UVDFND (NAME, 2, 'FREQ', INDX, IERR)
      IF ((INDX.GT.0) .AND. (IERR.EQ.0)) THEN
         CALL SECGET (NAME, 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL SECGET (NAME, 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         BCHAN = MAX (1, BCHAN)
         IF (ECHAN.LT.BCHAN) ECHAN = NAXIS(INDX)
         IF (BCHAN.GT.NAXIS(INDX)) BCHAN = 1
         ECHAN = MIN (ECHAN, NAXIS(INDX))
      ELSE
         BCHAN = 1
         ECHAN = 1
         END IF
      CALL SECPUT (NAME, 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL SECPUT (NAME, 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
      MSGTXT = 'OUVATT: ERROR ATTACHING AIPS FILE FOR ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUVATT: ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1001 FORMAT ('OUVATT: ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE OUCSET (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Sets file status
C   On READ the AIPS catalog status will be done by UVGET.
C   Inputs:
C      NAME     C*?   The name of the object.
C      STATUS   C*4   'READ', 'WRIT', 'DEST' (Write but destroy the file
C                     on error), RRAW (raw read - forces READ to be set)
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
C                                       AIPS status, on READ UVGET will
C                                       take care of this.
      IF (STATUS.NE.'READ') THEN
         IF (STATUS .EQ. 'RRAW') THEN
            CALL OUCFST (NAME, DISK, CNO, 'READ', IERR)
         ELSE
            CALL OUCFST (NAME, DISK, CNO, STATUS, IERR)
            END IF
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Set status
      CALL FSTGET (NAME, 'STATUS', TYPE, DIM, DUMMY, OLDSTA, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Ignore if 'DEST'
      IF (OLDSTA.NE.'DEST') THEN
         IF (STATUS .EQ. 'RRAW') THEN
            CALL FSTPUT (NAME, 'STATUS', OOACAR, DIM, DUMMY, 'READ',
     *         IERR)
            ELSE
               CALL FSTPUT (NAME, 'STATUS', OOACAR, DIM, DUMMY, STATUS,
     *            IERR)
               END IF
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE OUCCLR (NAME, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Clears file status.  Delete on fail status will be kept
C   On READ the AIPS catalog status will be done by UVGET.
C   Inputs:
C      NAME     C*?   The name of the object.
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   DISK, CNO, TYPE, DIM(3), DUMMY(1), BUFNO
      CHARACTER STATUS*4
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'UVDATA.INC'
C-----------------------------------------------------------------------
      IERR = 0
C
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Lookup STATUS
      CALL FSTGET (NAME, 'STATUS', TYPE, DIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Lookup DISK, CNO
      CALL OBDSKC (NAME, DISK, CNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       AIPS status, For READ done by
C                                       UVGET.
      IF ((STATUS.NE.'READ') .OR. UVRAW(BUFNO)) THEN
         CALL OUCFCL (NAME, DISK, CNO, STATUS, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
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
      SUBROUTINE OUCDES (NAME, STATUS, IERR)
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
      CALL OUCCLR (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set new status
      IF (STATUS.NE.'    ') THEN
         CALL OUCSET (NAME, STATUS, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE OUVSLT (NAME, NAME2, IERR)
C-----------------------------------------------------------------------
C   Public function
C   WARNING: THIS ROUTINE IS PROBABLY NOT NEEDED.  NAME2 WILL HAVE ITS
C   PARAMETERS SET WHEN OPENED TO THEIR OUTPUT FORMS - NAMELY WITH BCHAN
C   ETC APPLIED.  ONE NEEDS ONLY COPY NAME2'S HEADER PARAMETERS TO NAME
C   USING UVDCOP TO GET THE DESIRED RESULT.
C   Modify descriptors in NAME to correspond to selection criteria
C   in NAME2.  Supports selection by channel, IF and Stokes.
C   The frequency left in the uvdata descriptor is unchanged as this is
C   the frequency of the u,v and w.
C   Inputs:
C      NAME     C*?   The name of the object to be modified
C      NAME2    C*?   The name of the source object
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), NAME2*(*)
      INTEGER   IERR
C
      CHARACTER STOKES*4, CDUMMY*1
      INTEGER   TYPE, DIM(7), JLOCS, JLOCF, JLOCIF,  BIF, EIF, BCHAN,
     *   ECHAN, NAXIS(7), DUMMY(1), JLOCS2, JLOCF2, JLOCI2, NAXIS2(7)
      REAL      CRPIX(7), CRPIX2(7)
      DOUBLE PRECISION CRVAL(7)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Uv data pointers
      CALL UVDFND (NAME, 2, 'STOKES', JLOCS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDFND (NAME, 2, 'FREQ', JLOCF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDFND (NAME, 2, 'IF  ', JLOCIF, IERR)
      IF (IERR.NE.0) THEN
         JLOCIF = -1
         IERR = 0
         END IF
      CALL UVDFND (NAME2, 2, 'STOKES', JLOCS2, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDFND (NAME2, 2, 'FREQ', JLOCF2, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDFND (NAME2, 2, 'IF  ', JLOCI2, IERR)
      IF (IERR.NE.0) THEN
         JLOCIF = -1
         IERR = 0
         END IF
C                                       UV axis descriptor
C                                       NAXIS
      CALL UVDGET (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (NAME2, 'NAXIS', TYPE, DIM, NAXIS2, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       CRPIX
      CALL UVDGET (NAME, 'CRPIX', TYPE, DIM, CRPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (NAME2, 'CRPIX', TYPE, DIM, CRPIX2, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       CRVAL
      CALL UVDGET (NAME, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Selection from NAME2
C                                       BIF
      CALL SECGET (NAME2, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       EIF
      CALL SECGET (NAME2, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BCHAN
      CALL SECGET (NAME2, 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       ECHAN
      CALL SECGET (NAME2, 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       STOKES
      CALL SECGET (NAME2, 'STOKES', TYPE, DIM, DUMMY, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Apply selection: channel
      IF ((BCHAN.GT.NAXIS2(JLOCF2)) .OR. (BCHAN.LE.0)) BCHAN = 1
      IF ((ECHAN.GT.NAXIS2(JLOCF2)) .OR. (ECHAN.LE.0)) ECHAN =
     *   NAXIS2(JLOCF2)
      NAXIS(JLOCF) = ECHAN - BCHAN + 1
      CRPIX(JLOCF) = CRPIX2(JLOCF2) - BCHAN + 1
C                                       IF
      IF (JLOCI2.GT.0) THEN
         IF ((BIF.GT.NAXIS2(JLOCI2)) .OR. (BIF.LE.0)) BIF = 1
         IF ((EIF.GT.NAXIS2(JLOCI2)) .OR. (EIF.LE.0)) EIF =
     *      NAXIS2(JLOCI2)
      ELSE
         BIF = 1
         EIF = 1
         END IF
      IF (JLOCIF.GT.0) THEN
         NAXIS(JLOCIF) = EIF - BIF + 1
         CRPIX(JLOCIF) = CRPIX2(JLOCI2) - BIF + 1
         END IF
C                                       Stokes, not complete
      IF (STOKES(1:1).EQ.'I') THEN
         CRVAL(JLOCS) = 1.0D0
         NAXIS(JLOCS) = 1
      ELSE IF (STOKES(1:1).EQ.'Q') THEN
         CRVAL(JLOCS) = 2.0D0
         NAXIS(JLOCS) = 1
      ELSE IF (STOKES(1:1).EQ.'U') THEN
         CRVAL(JLOCS) = 3.0D0
         NAXIS(JLOCS) = 1
      ELSE IF (STOKES(1:1).EQ.'V') THEN
         CRVAL(JLOCS) = 4.0D0
         NAXIS(JLOCS) = 1
      ELSE IF (STOKES(1:1).EQ.'R') THEN
         CRVAL(JLOCS) = -1.0D0
         NAXIS(JLOCS) = 1
      ELSE IF (STOKES(1:1).EQ.'L') THEN
         CRVAL(JLOCS) = -2.0D0
         NAXIS(JLOCS) = 1
         END IF
C                                       Update UV axis descriptor
      DIM(1) = 7
      DIM(2) = 1
      DIM(3) = 0
      CALL UVDPUT (NAME, 'NAXIS', OOAINT, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDPUT (NAME, 'CRVAL', OOADP, DIM, CRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDPUT (NAME, 'CRPIX', OOARE, DIM, CRPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       update selection in output
      BCHAN = 1
      BIF = 1
      ECHAN = NAXIS(JLOCF)
      IF (JLOCI2.GT.0) EIF = NAXIS(JLOCIF)
      DIM(1) = 1
      CALL SECPUT (NAME, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL SECPUT (NAME, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL SECPUT (NAME, 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL SECPUT (NAME, 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE OUVCGT (NAME, CAT, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Return catalog header record for an uvdata indicating correct
C   compression etc..  This function should be used to get the CATBLK to
C   be passed to nonOOP routines.
C   Inputs:
C      NAME     C*?    The name of the uvdata object.
C   Outputs:
C      CAT      I(256) Catalog header record
C      IERR     I      Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   CAT(*), IERR
C
      INTEGER   DISK, CNO
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup DISK, CNO
      CALL OBDSKC (NAME, DISK, CNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get catalog header.
      CALL CATIO ('READ', DISK, CNO, CAT, 'REST', SBUFF, IERR)
C                                       Damn catalog flags
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1001) IERR
         GO TO 990
         END IF
      IERR = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('OUVCGT: ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE OUCFST (NAME, DISK, CNO, STATUS, IERR)
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
      SUBROUTINE OUCFCL (NAME, DISK, CNO, STATUS, IERR)
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
      CHARACTER NAME*(*), STATUS*4
      INTEGER   DISK, CNO, IERR
C
      INTEGER   SEQI, BUFF(256), LOOP, IRW, HIGH, INDEX
      CHARACTER NAMI*12, CLAI*6, PTYPE*2, STAT*4
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
      SUBROUTINE OUCREA (NAME, IERR)
C-----------------------------------------------------------------------
C   Private
C   Creates a file structure for uv data set "name".  Uses Name and
C   header associated with "name".
C
C   If the file does not already exist and if ISCOMP is true for the
C   UVDESC object belonging to "name" then the new file will be a
C   compressed UV file unless NCORR is 1. If NCORR is 1 then ISCOMP will
C   automatically be changed to false since there is no space to be
C   saved by compressing the data.
C
C   Inputs:
C      NAME    C*?   The name of the object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   SEQO, DISKO, CNOO, DIM(7), TYPE, GC, DUMMY(1), NCORR,
     *   MSGSAV
      LOGICAL   EXISTS, ISCOMP, LDUMMY(1)
      CHARACTER STATUS*4, NAMCLT*20, NAMO*12, CLAO*6, ANAME*8, FNAME*48,
     *   CDUMMY*1
      EQUIVALENCE (LDUMMY, DUMMY)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Add data items for compression
      CALL UVDGET (NAME, 'NCORR', TYPE, DIM, NCORR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (NAME, 'ISCOMP', TYPE, DIM, ISCOMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear compression flag if there
C                                       is only one correlator
      IF (NCORR.EQ.1) THEN
         ISCOMP = .FALSE.
         CALL UVDPUT (NAME, 'ISCOMP', TYPE, DIM, ISCOMP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
      IF (ISCOMP) THEN
         CALL OUVPAK (NAME, .TRUE., IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Get header
      CALL OBHGET (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Reset relevant information
      CATBLK(KINIT) = 0
      CATBLK(KITYP) = 0
      CATBLK(KRBMJ) = 0.0
      CATBLK(KRBMN) = 0.0
      CATBLK(KRBPA) = 0.0
C                                       If no. vis. .LE. 0 use 1000
      IF (CATBLK(KIGCN).LE.0) CATBLK(KIGCN) = 1000
C                                       Get disk number
      CALL FNAGET (NAME, 'DISK', TYPE, DIM, DISKO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Create file.
      CNOO = 1
      STATUS = 'DEST'
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL UVCREA (DISKO, CNOO, SBUFF, IERR)
      MSGSUP = MSGSAV
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
C                                       Set FILFILE flag
      DIM(1) = 1
      DIM(2) = 1
      LDUMMY(1) = EXISTS
      CALL OUVPUT (NAME, 'OLDFILE', OOALOG, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
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
C                                       Save header
      CALL OBHPUT (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Save CNO, actual DISK
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL FNAPUT (NAME, 'DISK', OOAINT, DIM, DISKO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      SEQO = CATBLK(KIIMS)
      CALL FNAPUT (NAME, 'IMSEQ', OOAINT, DIM, SEQO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FNAPUT (NAME, 'CNO', OOAINT, DIM, CNOO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Name
      CALL FNAGET (NAME, 'NAMCLSTY', TYPE, DIM, DUMMY, NAMCLT, IERR)
      IF (IERR.NE.0) GO TO 995
      NAMO = NAMCLT(1:12)
      CLAO = NAMCLT(13:18)
      DIM(1) = 12
      CALL FNAPUT (NAME, 'NAME', OOACAR, DIM, DUMMY, NAMO, IERR)
      IF (IERR.NE.0) GO TO 995
      DIM(1) = 6
      CALL FNAPUT (NAME, 'CLASS', OOACAR, DIM, DUMMY, CLAO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       UV descriptor file info
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
C                                       FDISK
      CALL UVDPUT (NAME, 'FDISK', OOAINT, DIM, DISKO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Mark as empty
      GC = 0
      CALL UVDPUT (NAME, 'GCOUNT', OOAINT, DIM, GC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       FNAME
      CALL ZPHFIL ('UV', DISKO, CNOO, 1, FNAME, IERR)
      DIM(1) = 48
      CALL UVDPUT (NAME, 'FNAME', OOACAR, DIM, DUMMY, FNAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       ANAME
      ANAME = '  '
      DIM(1) = 8
      CALL UVDPUT (NAME, 'ANAME', OOACAR, DIM, DUMMY, ANAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       File status init 'WRIT'
      STATUS = 'WRIT'
      DIM(1) = 4
      CALL FSTPUT (NAME, 'STATUS', OOACAR, DIM, DUMMY, STATUS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear File status (undo UVCREA
C                                       status)
      CALL OUCCLR (NAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Set destroy on fail flag
      STATUS = 'DEST'
      IF (EXISTS) STATUS = 'WRIT'
C???      CALL OUCSET (NAME, STATUS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Mark as invalid.
      DIM(1) = 1
      CALL FSTPUT (NAME, 'VALID', OOALOG, DIM, .FALSE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (7)
 995  MSGTXT = 'OUCREA: CREATING ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('OUCREA: ERROR',I3,' CREATING OUTPUT FILE')
 1003 FORMAT ('OUCREA: ERROR',I3,' READING OLD CATBLK')
      END
      SUBROUTINE OUBGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Private
C   If KEYWRD refers to a recognized member base class then fetch the
C   value (array) for a specified member
C   Note: for KEYWRD='VIS' VALUE is tha rp array and VALUEC is the
C   visibility array.
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
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   POINT, NMEMS, IMEM, LOOP, JERR
      PARAMETER (NMEMS = 7)
      CHARACTER MEMBER*16, MEMS(NMEMS)*16
      INCLUDE 'INCS:DMSG.INC'
      DATA MEMS /'VIS', 'FILE_NAME', 'FILE_STATUS',  'UV_DESC',
     *   'VELOCITY', 'POSITION', 'CALEDIT'/
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
C                                       Vis data: not work here
 110     JERR = 99
         MSGTXT = 'OUBGET: MEMBER VIS SPECIFIED - CANNOT WORK'
         CALL MSGWRT (8)
C                                       was:
C        CALL UVREAD (NAME, VALUE, VALUEC, JERR)
         TYPE = 0
         GO TO 900
C                                       File name
 120     CALL FNAGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       File Status
 130     CALL FSTGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       UVdata description
 140     CALL UVDGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
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
C                                       sel/cal/edit
 170     CALL SECGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
 900  IERR = 0
      IF (JERR.NE.0) IERR = 2
C
 999  RETURN
      END
      SUBROUTINE OUBPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Private
C   If KEYWRD refers to a recognized member base class then store the
C   value (array) for a specified member
C   Note: for KEYWRD='VIS' VALUE is tha rp array and VALUEC is the
C   visibility array.
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
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   POINT, NMEMS, IMEM, LOOP, JERR
      PARAMETER (NMEMS = 7)
      CHARACTER MEMBER*16, MEMS(NMEMS)*16
      INCLUDE 'INCS:DMSG.INC'
      DATA MEMS /'VIS', 'FILE_NAME', 'FILE_STATUS',  'UV_DESC',
     *   'VELOCITY', 'POSITION', 'CALEDIT'/
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
      GO TO (110, 120, 130, 140, 150, 160, 170),  IMEM
C                                       Vis data: not work here
 110     JERR = 99
         MSGTXT = 'OUBPUT: MEMBER VIS SPECIFIED - CANNOT WORK'
         CALL MSGWRT (8)
C                                       was:
C        CALL UVWRIT (NAME, VALUE, VALUEC, JERR)
         GO TO 900
C                                       File name
 120     CALL FNAPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       File Status
 130     CALL FSTPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
C                                       UVdata description
 140     CALL UVDPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
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
C                                       sel/cal/edit
 170     CALL SECPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
 900  IERR = 0
      IF (JERR.NE.0) IERR = 2
C
 999  RETURN
      END
      SUBROUTINE OUVXPN (NAME, NVADD, IERR)
C-----------------------------------------------------------------------
C   Private
C   Expands UV data file associated with object NAME by NVADD vis.
C   Note: file must be open.
C   Inputs:
C      NAME    C*?   The name of the object.
C      NVADD   I     The number of visibilities to add
C   Outputs:
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   NVADD, IERR
      CHARACTER NAME*(*)
C
      INTEGER   BUFNO, TYPE, DIM(7), FDISK, NREC, DUMMY(1)
      CHARACTER FNAME*48, CDUMMY*1
      INCLUDE 'UVDATA.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get buffer no.
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       File name, disk
      CALL UVDGET (NAME, 'FNAME', TYPE, DIM, DUMMY, FNAME, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (NAME, 'FDISK', TYPE, DIM, FDISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Expand file
C                                       Add NVADD vis.
      NREC = ((NVADD * UVLREC(BUFNO) * 2) / 512) + 1
      CALL ZEXPND  (OBJLUN(BUFNO), FDISK, FNAME, NREC, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'OUVXPN: FILE EXPANSION FAILED - DISK FULL'
         GO TO 990
         END IF
C                                       Get new file size, in vis
      CALL ZEXIST (FDISK, FNAME, UVSIZE(BUFNO), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      UVSIZE(BUFNO) = (UVSIZE(BUFNO) * 256.0D0) / UVLREC(BUFNO)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'OUVXPN: ERROR EXPANDING UV: ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUVXPN: ZEXIST ERROR:', I3)
      END
      SUBROUTINE OUVCPR (NAME, IERR)
C-----------------------------------------------------------------------
C   Private
C   Compress UV data file associated with object NAME to size needed.
C   Uses GCOUNT from UV_DESC object to determine actual size.
C   Note: file must be open.
C   Inputs:
C      NAME    C*?   The name of the object.
C   Outputs:
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER NAME*(*)
C
      INTEGER   BUFNO, TYPE, DIM(7), FDISK, FCNO, GCOUNT
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'UVDATA.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get buffer no.
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       File disk, cno, GCOUNT
      CALL OBDSKC (NAME, FDISK, FCNO, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (NAME, 'GCOUNT', TYPE, DIM, GCOUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get catalog header
      CALL OBHGET (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set old number of visibilities
      CATBLK(KIGCN) = UVSIZE(BUFNO)
C                                       Compress
      CALL UCMPRS (GCOUNT, FDISK, FCNO, OBJLUN(BUFNO), CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'OUVCPR: ERROR COMPRESSING UV: ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUVCPR: UCMPRS ERROR:', I3)
      END
      SUBROUTINE OUVTNF (CATIN, CATOUT, IERR)
C-----------------------------------------------------------------------
C   UV data utility routine
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
               MSGTXT = 'OUVTNF: TOO MANY TABLE TYPES'
               CALL MSGWRT (6)
               IERR = 1
               END IF
            END IF
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE OUVPAK (NAME, PACK, IERR)
C-----------------------------------------------------------------------
C   Private.
C   Set or unset axis dimensions and random parameters for packed uv
C   data.
C
C   If PACK is true then NAXIS(1) will be set to 1 and WEIGHT and SCALE
C   will be added to the random parameter list.
C   If PACK is false then NAXIS(1) will be set to 3 and WEIGHT and SCALE
C   will be removed from the random parameter list.
C
C   Inputs:
C      NAME    C*?       The name of the object
C      PACK    L         Turn on packing?
C
C   Outputs:
C      IERR    I         Status: 0 => completed successfully
C                                anything else => error
C-----------------------------------------------------------------------
      CHARACTER NAME *(*)
      LOGICAL   PACK
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(3), NAXIS(7), NRPARM, DUMMY(1), WTIDX
      CHARACTER PTYPE(14)*8, CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (PACK) THEN
         CALL UVDGET (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         NAXIS(1) = 1
         CALL UVDPUT (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL UVDFND (NAME, 1, 'WEIGHT', WTIDX, IERR)
         IF (IERR.EQ.1) IERR = 0
         IF (IERR.NE.0) GO TO 995
         IF (WTIDX.LE.0) THEN
            CALL UVDGET (NAME, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            CALL UVDGET (NAME, 'PTYPE', TYPE, DIM, DUMMY, PTYPE, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Reuse removed parameter slots
C                                       when possible
   10       IF (PTYPE(NRPARM).EQ.'REMOVED ') THEN
               NRPARM = NRPARM - 1
               GO TO 10
               END IF
            IF (NRPARM.GT.12) THEN
               MSGTXT = 'OUVPAK: ' //
     *            'NO SPACE FOR PARAMETERS TO SUPPORT COMPRESSED DATA'
               CALL MSGWRT (7)
               IERR = 1
               GO TO 995
               END IF
            NRPARM = NRPARM + 2
            PTYPE(NRPARM-1) = 'WEIGHT  '
            PTYPE(NRPARM)   = 'SCALE   '
            CALL UVDPUT (NAME, 'PTYPE', TYPE, DIM, DUMMY, PTYPE, IERR)
            IF (IERR.NE.0) GO TO 995
            DIM(1) = 1
            DIM(2) = 1
            DIM(3) = 0
            CALL UVDPUT (NAME, 'NRPARM', OOAINT, DIM, NRPARM, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
      ELSE
         CALL UVDGET (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         NAXIS(1) = 3
         CALL UVDPUT (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL UVDFND (NAME, 1, 'WEIGHT', WTIDX, IERR)
         IF (IERR.EQ.1) IERR = 0
         IF (IERR.NE.0) GO TO 995
         IF (WTIDX.GT.0) THEN
            CALL UVDGET (NAME, 'PTYPE', TYPE, DIM, DUMMY, PTYPE, IERR)
            IF (IERR.NE.0) GO TO 995
            PTYPE(WTIDX)     = 'REMOVED '
            PTYPE(WTIDX + 1) = 'REMOVED '
            CALL UVDPUT (NAME, 'PTYPE', TYPE, DIM, DUMMY, PTYPE, IERR)
            IF (IERR.NE.0) GO TO 995
            CALL UVDGET (NAME, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            IF (WTIDX.EQ.(NRPARM - 1)) THEN
               NRPARM = NRPARM - 2
               CALL UVDPUT (NAME, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 995
               END IF
            END IF
         END IF
      GO TO 999
C
  995 MSGTXT = 'OUVPAK: ERROR SETTING PACKING STATE FOR ' // NAME
      CALL MSGWRT (7)
C
  999 RETURN
      END

