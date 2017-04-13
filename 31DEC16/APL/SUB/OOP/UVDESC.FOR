C   UV data descriptor class:  name = 'UV_DESC'
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "Uv data Descriptor" class library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2015
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
C   The uv data descriptor contains descriptive information about an
C   uv data set.
C
C   uv data may be compressed on disk by packing each real and imaginary
C   component into a single word as scaled integers and adding two extra
C   random parameters that carry weight and scaling information.  The
C   dimension of the COMPLEX axis is set to 1 for compressed data.  If
C   a compressed data set is opened for reading, however, NAXIS(1) will
C   be set to 3 and the additional random parameters will be stripped
C   off so the the uv descriptor reflects the uncompressed data as
C   returned by UVREAD.  The only indication that the data is actually
C   compressed is the value of ISCOMP.  If a compressed file is opened
C   for writing the descriptor will usually reflect the compressed
C   state of the data (ie. NAXIS(1) = 1 and the WEIGHT and SCALE
C   parameters will be included in NRPARM  and PTYPE).  You may create
C   a compressed output file by setting ISCOMP to true before creating
C   the disk resident structures (usually in OUVOPN); NAXIS(1) will be
C   automatically set to 1 and the extra parameters added if not already
C   present.
C
C   Class public members;
C      OBJECT   C*8    Source name
C      TELESCOP C*8    Telescope name
C      INSTRUME C*8    Receiver name
C      OBSERVER C*8    Observer name
C      DATE-OBS C*8    Observing date as dd/mm/yy
C      DATE-MAP C*8    Creation date as dd/mm/yy
C      BUNIT    C*8    Units of the correlation array
C      EPOCH    R      Mean epoch of celestial position (1950, 2000)
C      USERNO   I      User ID number
C      NDIM     I      Number of dimensions in the array
C      NAXIS    I(*)   Dimension of each axis
C      CTYPE    C*8(*) Label for each axis
C      CRVAL    D(*)   Coordinate value at reference pixel
C      CDELT    R(*)   Coordinate increment
C      CRPIX    R(*)   Reference pixel for axis
C      CROTA    R(*)   Coordinate rotation for each axis.
C      GCOUNT   I      Number of visibilities
C      SORTORD  C*2    Sort order.
C      NRPARM   I      Number of random parameters
C      PTYPE    C*8(*) Random parameter labels
C      LREC     I      Visibility record length.
C      TYPEUVD  C*2    Type of uv data: UV => vis, SD = singledish
C      NCORR    I      Number of correlators in visibility
C      VISOFF   I      Offset in visibility number (valid on read only
C                      for single source datasets)
C      ISCOMP   L      If true, data on disk are in compressed format.
C      ANAME    C*8    Name of array if memory resident
C      FNAME    C*48?  Physical name of array file if disk resident.
C      FDISK    I      Disk number for FNAME.
C      REFFREQ  D      Reference frequency (that of u,v,w) in Hz.
C      REFFPIX  R      Pixel of reference freq in u,v,w
C
C   Public functions:
C      UVDGET (name, keywrd, type, dim, value, valuec, ierr)
C         Return image descriptor member.
C      UVDPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Store image descriptor member.
C      UVDPNT (name, ilocu, ilocv, ilocw, iloct, ilocb, ilocsu, ilocfq,
C     *   iloca1, iloca2, ilocsa, jlocc, jlocs, jlocf, jlocr, jlocd,
C     *   jlocif, incs, incf, incif, ierr)
C         Returns pointers etc for interpreting a visibility data
C         record.
C      UVDCOP (namein, namout, ierr)
C         Copy UV descriptor info from namein to namout.
C      UVDSCP (namein, namout, ierr)
C         Copy UV descriptor info not related to object size.
C      UVDFND (name, itype, label, index, ierr)
C         Look up the random parameter or axis "label" number
C
LOCAL INCLUDE 'UV_DESC.INC'
C                                       UV_DESC class include
      INTEGER   NMEML
      PARAMETER (NMEML = 33)
      CHARACTER MEMS(NMEML)*8, THSCLS*16
      DATA MEMS /'OBJECT','TELESCOP','INSTRUME', 'OBSERVER', 'DATE-OBS',
     *   'DATE-MAP', 'BUNIT', 'EPOCH', 'USERNO', 'NDIM', 'NAXIS',
     *   'CTYPE', 'CRVAL', 'CDELT', 'CRPIX', 'CROTA', 'GCOUNT',
     *   'SORTORD', 'NRPARM', 'PTYPE', 'LREC', 'NCORR', 'TYPEUVD',
     *   'VISOFF', 'ISCOMP', 'ANAME',  'FNAME', 'FDISK', 'REFFREQ',
     *   'REFFPIX', 'BMAJ', 'BMIN', 'BPA'/
      DATA THSCLS /'UV_DESC'/
LOCAL END
      SUBROUTINE UVDGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Return UV data descriptor member.
C   Inputs:
C      NAME    C*?    Object name
C      KEYWRD  C*(*)  Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of the array.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.,
C                    2= Input error.
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER    IMEM, LOOP, OBJNUM, POINT
      CHARACTER MEMBER*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UV_DESC.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       No base classes exist for this
C                                       class.
      IF (POINT.GE.1) THEN
         IERR = 2
         MSGTXT = 'NO BASE CLASSES FOR CLASS ' // THSCLS
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Save member name
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized
C                                       members.
      IMEM = -1
      DO 10 LOOP = 1,NMEML
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       Find it?
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' MEMBER ' // MEMBER
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Fetch value:
         CALL OBGET (OBJNUM, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      IERR)
C                                       Message if not found
         IF (IERR.EQ.1) THEN
            MSGTXT = 'MEMBER ' // MEMBER // ' NOT FOUND'
            CALL MSGWRT (6)
            MSGTXT = 'OBJECT =' // NAME
            CALL MSGWRT (6)
            END IF
C
 999  RETURN
      END
      SUBROUTINE UVDPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Store UV data descriptor member.
C   Inputs:
C      NAME    C*?    Object name
C      KEYWRD  C*(*)  Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of the array.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.,
C                    2= Input error.
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   IMEM, LOOP, OBJNUM, POINT
      CHARACTER MEMBER*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UV_DESC.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       No base classes exist for this
C                                       class.
      IF (POINT.GE.1) THEN
         IERR = 2
         MSGTXT = 'NO BASE CLASSES FOR CLASS ' // THSCLS
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Save member name
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized
C                                       members.
      IMEM = -1
      DO 10 LOOP = 1,NMEML
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       Find it?
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' MEMBER ' // MEMBER
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Fetch value:
         CALL OBPUT (OBJNUM, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      IERR)
C                                       Message if not stored
         IF (IERR.NE.0) THEN
            MSGTXT = 'MEMBER ' // MEMBER // ' COULD NOT BE STORED'
            CALL MSGWRT (6)
            MSGTXT = 'OBJECT =' // NAME
            CALL MSGWRT (6)
            END IF
C
 999  RETURN
      END
      SUBROUTINE UVDPNT (NAME, IILOCU, IILOCV, IILOCW, IILOCT, IILOCB,
     *   IILCSU, IILCFQ, IILCA1, IILCA2, IILCSA, JJLOCC, JJLOCS, JJLOCF,
     *   JJLOCR, JJLOCD, JJLCIF, IINCS, IINCF, IICIF, IERR)
C-----------------------------------------------------------------------
C   Public
C   Returns pointers etc. for a uv data set.
C   Note: pointers are 1-rel unlike UVPGET which returns 0-rel pointers.
C
C   Increments are applicable to uncompressed data; data returned by the
C   object system are always uncompressed automatically.
C
C   Inputs:
C      NAME    C*?    Object name
C   Outputs:
C      IILOCU    I   Offset from beginning of vis record of U
C                    or longitude for single dish format data.
C      IILOCV    I   Offset from beginning of vis record of V
C                    or longitude for single dish format data.
C      IILOCW    I   Offset from beginning of vis record of W.
C      IILOCT    I                 "                      Time
C      IILOCB    I                 "                    Baseline
C      IILCSU    I                 "                    Source id.
C      IILCFQ    I                 "                    Freq id.
C      IILCA1    I                 "                    antenna 1
C      IILCA2    I                 "                    antenna 2
C      IILCSA    I                 "                    subarray
C      JJLOCC    I   0-rel. order in data of complex values
C      JJLOCS    I   Order in data of Stokes' parameters.
C      JJLOCF    I   Order in data of Frequency.
C      JJLOCR    I   Order in data of RA
C      JJLOCD    I   Order in data of dec.
C      JJLCIF    I   Order in data of IF.
C      IINCS     I   Increment in data for stokes (see above)
C      IINCF     I   Increment in data for freq. (see above)
C      IICIF     I   Increment in data for IF.
C      IERR      I   Error code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   IILOCU, IILOCV, IILOCW, IILOCT, IILOCB, IILCSU, IILCFQ,
     *   IILCA1, IILCA2, IILCSA, JJLOCC, JJLOCS, JJLOCF, JJLOCR, JJLOCD,
     *   JJLCIF, IINCS, IINCF, IICIF, IERR
      CHARACTER NAME*(*)
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get CATBLK
      CALL OBHGET (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Use UVPGET to crack
      CALL UVPGET (IERR)
      IF (ILOCU.GE.0) IILOCU = ILOCU + 1
      IF (ILOCV.GE.0) IILOCV = ILOCV + 1
      IF (ILOCW.GE.0) IILOCW = ILOCW + 1
      IF (ILOCT.GE.0) IILOCT = ILOCT + 1
      IF (ILOCB.GE.0) IILOCB = ILOCB + 1
      IF (ILOCSU.GE.0) IILCSU = ILOCSU + 1
      IF (ILOCFQ.GE.0) IILCFQ = ILOCFQ + 1
      IF (ILOCA1.GE.0) IILCA1 = ILOCA1 + 1
      IF (ILOCA2.GE.0) IILCA2 = ILOCA2 + 1
      IF (ILOCSA.GE.0) IILCSA = ILOCSA + 1
      IF (JLOCC.GE.0) JJLOCC = JLOCC + 1
      IF (JLOCS.GE.0) JJLOCS = JLOCS + 1
      IF (JLOCF.GE.0) JJLOCF = JLOCF + 1
      IF (JLOCR.GE.0) JJLOCR = JLOCR + 1
      IF (JLOCD.GE.0) JJLOCD = JLOCD + 1
      IF (JLOCIF.GE.0) JJLCIF = JLOCIF + 1
      IINCS = INCS
      IINCF = INCF
      IICIF = INCIF
C                                       Compensate for any compression
C                                       used in the disk file
      IF (CATBLK(KINAX + JLOCC) .EQ. 1) THEN
         IF (JLOCS .GT. JLOCC) THEN
            IINCS = 3 * INCS
            END IF
         IF (JLOCF .GT. JLOCC) THEN
            IINCF = 3 * INCF
            END IF
         IF (JLOCIF .GT. JLOCC) THEN
            IICIF = 3 * INCIF
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE UVDCOP (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copy all non derived keywords of UV_DESC except file name.
C   Also copies associated VELOCITY and POSITION objects.
C   Inputs:
C      NAMEIN  C*?    Input object name
C      NAMOUT  C*?    Output object name
C   Outputs:
C      IERR    I     Error code, 0=OK.  1=> didn't find.,
C                    2= Input error.
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER NAMEIN*(*), NAMOUT*(*)
C
      INTEGER NDER, NPOS, NVEL
C                                       NDER = no. derived keywords, and
C                                       others not to copy.
      PARAMETER (NDER=6)
C                                       NPOS = no. POSITION to copy
      PARAMETER (NPOS = 4)
C                                       NVEL = no. VELOCITY to copy
      PARAMETER (NVEL = 4)
      INTEGER   TYPE, DIM(7), LOOP, ID, ILOCF, DUMMY(2)
      DOUBLE PRECISION REFREQ, DVAL(50), DDUMMY(1)
      CHARACTER CVAL*200, DERMEM(NDER)*8, CPOS(NPOS)*8, CVEL(NVEL)*8,
     *   CDUMMY*1
      LOGICAL   ISDER
      REAL      REFPIX, RDUMMY(2)
      EQUIVALENCE (DVAL, DDUMMY, RDUMMY, DUMMY)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UV_DESC.INC'
      DATA DERMEM /'LREC', 'NCORR', 'TYPEUVD', 'ANAME',' FNAME',
     *   'FDISK'/
      DATA CPOS /'OBSRA', 'OBSDEC', 'XSHIFT', 'YSHIFT'/
      DATA CVEL /'VELREF' ,'ALTRVAL', 'ALTRPIX', 'RESTFREQ'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Copy position
      DO 30 LOOP = 1,NPOS
         CALL PSNGET (NAMEIN, CPOS(LOOP), TYPE, DIM, DVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL PSNPUT (NAMOUT, CPOS(LOOP), TYPE, DIM, DVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
 30      CONTINUE
C                                       Copy VELOCITY
      DO 40 LOOP = 1,NVEL
         CALL VELGET (NAMEIN, CVEL(LOOP), TYPE, DIM, DVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL VELPUT (NAMOUT, CVEL(LOOP), TYPE, DIM, DVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
 40      CONTINUE
C                                       Lookup actual frequency info
      CALL UVDFND (NAMEIN, 2, 'FREQ', ILOCF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR FINDING INPUT FREQUENCY AXIS'
         CALL MSGWRT (7)
         GO TO 995
         END IF
C                                       Copy it tp output
      CALL UVDGET (NAMEIN, 'REFFREQ ', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      REFREQ = DDUMMY(1)
      CALL UVDPUT (NAMOUT, 'REFFREQ ', OOADP, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (NAMEIN, 'REFFPIX ', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      REFPIX = RDUMMY(1)
      CALL UVDPUT (NAMOUT, 'REFFPIX ', OOARE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Loop over class members
      DO 100 LOOP = 1,NMEML
C                                       Can't do derived keywords
         ISDER = .FALSE.
         DO 50 ID = 1,NDER
            ISDER = ISDER .OR. (MEMS(LOOP).EQ.DERMEM(ID))
 50         CONTINUE
         IF (.NOT.ISDER) THEN
            CALL UVDGET (NAMEIN, MEMS(LOOP), TYPE, DIM, DUMMY, CVAL,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Special trap for frequency
            IF (MEMS(LOOP).EQ.'CRVAL') DVAL(ILOCF) = REFREQ
            CALL UVDPUT (NAMOUT, MEMS(LOOP), TYPE, DIM, DUMMY, CVAL,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
 100     CONTINUE
      GO TO 999
C                                       Error
 995  MSGTXT = 'ERROR COPYING UV_DESC ' // NAMEIN
      CALL MSGWRT (8)
      MSGTXT = ' TO ' // NAMOUT
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE UVDSCP (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copy selected portion of UV_DESC not related to size or file name.
C   Also copies associated VELOCITY and POSITION objects.
C   Inputs:
C      NAMEIN  C*?    Input object name
C      NAMOUT  C*?    Output object name
C   Outputs:
C      IERR    I     Error code, 0=OK.  1=> didn't find.,
C                    2= Input error.
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER NAMEIN*(*), NAMOUT*(*)
C
      INTEGER NDER, NPOS, NVEL
C                                       NDER = no. derived keywords, and
C                                       keywords not to copy.
      PARAMETER (NDER=13)
C                                       NPOS = no. POSITION to copy
      PARAMETER (NPOS = 4)
C                                       NVEL = no. VELOCITY to copy
      PARAMETER (NVEL = 4)
      INTEGER   TYPE, DIM(7), LOOP, IVAL(50), ID, ILOCF
      REAL      RVAL(50)
      REAL      REFPIX
      DOUBLE PRECISION DVAL(25), REFREQ
      CHARACTER CVAL*200, DERMEM(NDER)*8, CPOS(NPOS)*8, CVEL(NVEL)*8,
     *   CDUMMY*1
      LOGICAL   ISDER
      EQUIVALENCE (IVAL, RVAL, DVAL)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UV_DESC.INC'
      DATA DERMEM /'LREC', 'NCORR', 'TYPEUVD', 'GCOUNT', 'VISOFF',
     *   'NDIM', 'NAXIS', 'NRPARM', 'ISCOMP', 'ANAME', 'FNAME', 'FDISK',
     *   'PTYPE'/
      DATA CPOS /'OBSRA', 'OBSDEC', 'XSHIFT', 'YSHIFT'/
      DATA CVEL /'VELREF' ,'ALTRVAL', 'ALTRPIX', 'RESTFREQ'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Copy position
      DO 30 LOOP = 1,NPOS
         CALL PSNGET (NAMEIN, CPOS(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL PSNPUT (NAMOUT, CPOS(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
 30      CONTINUE
C                                       Copy VELOCITY
      DO 40 LOOP = 1,NVEL
         CALL VELGET (NAMEIN, CVEL(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL VELPUT (NAMOUT, CVEL(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
 40      CONTINUE
C                                       Lookup actual frequency
C                                       information.
      CALL UVDFND (NAMEIN, 2, 'FREQ', ILOCF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR FINDING INPUT FREQUENCY AXIS'
         CALL MSGWRT (7)
         GO TO 995
         END IF
C                                       Copy it tp output
      CALL UVDGET (NAMEIN, 'REFFREQ ', TYPE, DIM, IVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      REFREQ = DVAL(1)
      CALL UVDPUT (NAMOUT, 'REFFREQ ', OOADP, DIM, IVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (NAMEIN, 'REFFPIX ', TYPE, DIM, IVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      REFPIX = RVAL(1)
      CALL UVDPUT (NAMOUT, 'REFFPIX ', OOARE, DIM, IVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Loop over class members
      DO 100 LOOP = 1,NMEML
C                                       Can't do derived keywords
         ISDER = .FALSE.
         DO 50 ID = 1,NDER
            ISDER = ISDER .OR. (MEMS(LOOP).EQ.DERMEM(ID))
 50         CONTINUE
         IF (.NOT.ISDER) THEN
            CALL UVDGET (NAMEIN, MEMS(LOOP), TYPE, DIM, IVAL, CVAL,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Special trap for frequency
            IF (MEMS(LOOP).EQ.'CRVAL') DVAL(ILOCF) = REFREQ
            CALL UVDPUT (NAMOUT, MEMS(LOOP), TYPE, DIM, IVAL, CVAL,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
 100     CONTINUE
      GO TO 999
C                                       Error
 995  MSGTXT = 'ERROR COPYING UV_DESC ' // NAMEIN
      CALL MSGWRT (8)
      MSGTXT = ' TO ' // NAMOUT
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE UVDFND (NAME, ITYPE, LABEL, INDEX, IERR)
C-----------------------------------------------------------------------
C   Public
C   Look up the random parameter or axis "label" number
C   Inputs:
C      NAME    C*?    Object name
C      ITYPE   I      1 = random parameter, 2 = axis
C      LABEL   C*(*)  Label, only length passed is compared.
C   Outputs:
C      INDEX   I     Random parameter number or array axis.
C                    -1 => not found
C      IERR    I     Error code, 0=OK.  1=> didn't find., otherwise
C                    error.
C-----------------------------------------------------------------------
      INTEGER   ITYPE, INDEX, IERR
      CHARACTER NAME*(*), LABEL*(*)
C
      INTEGER   LOOP, TYPE, DIM(7), SLEN, NCHK, OBJNUM, DUMMY
      CHARACTER CLABEL(20)*8, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 1
      INDEX = -1
C                                       Length of string to compare
      SLEN = LEN (LABEL)
      IF (SLEN.LE.0) GO TO 999
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Fetch info by ITYPE
C                                       Random parameters
      IF (ITYPE.EQ.1) THEN
         CALL OBGET (OBJNUM, 'PTYPE', TYPE, DIM, DUMMY, CLABEL, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBGET (OBJNUM, 'NRPARM', TYPE, DIM, NCHK, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Axis
      ELSE IF (ITYPE.EQ.2) THEN
         CALL OBGET (OBJNUM, 'CTYPE', TYPE, DIM, DUMMY, CLABEL, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBGET (OBJNUM, 'NDIM', TYPE, DIM, NCHK, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         WRITE (MSGTXT,1000) ITYPE
         CALL MSGWRT (8)
         IERR = 2
         GO TO 999
         END IF
C                                       assume no match
      IERR = 1
C                                       Check for match, take last one
      DO 100 LOOP = 1,NCHK
         IF (LABEL.EQ.CLABEL(LOOP)(1:SLEN)) THEN
            INDEX = LOOP
            IERR = 0
            END IF
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVDFND: UNKNOWN TYPE CODE: ',I5)
      END
