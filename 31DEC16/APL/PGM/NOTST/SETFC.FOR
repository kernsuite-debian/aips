LOCAL INCLUDE 'SETFC.INC'
      HOLLERITH XNAMEI(3), XCLASI(2), XBXFIL(12), XXSOUR(4,30),
     *   XINFIL(12), XOPTYP
      REAL       DISKIN, XSEQ, XBCNT, CELSIZ(2), XSIZE(2), SHIFT(2),
     *   BFLUX, BPARM(10), PBPARM(7)
      COMMON /INPARM/ XNAMEI, XCLASI, XSEQ, DISKIN, XXSOUR, XBCNT,
     *   XBXFIL, CELSIZ, XSIZE, SHIFT, BFLUX, BPARM, XOPTYP, PBPARM,
     *   XINFIL
C
      DOUBLE PRECISION RA0, DEC0
      INTEGER   IMSIZE(2), OVRLAP, CATNO, SEQIN, IDISK, LUNBX, LUNNV,
     *   OFIELD, INDBX, INDNV, WRBOX
      LOGICAL   RQUICK
      CHARACTER BOXFIL*48, NAMEIN*12, CLASIN*6, NVSSF*48, OPTYPE*4
      COMMON /SETFCP/ IMSIZE, OVRLAP, CATNO, SEQIN, IDISK, LUNBX, LUNNV,
     *   RQUICK, OFIELD, INDBX, INDNV, WRBOX
      COMMON /SETFCC/ BOXFIL, NAMEIN, CLASIN, NVSSF, OPTYPE
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
LOCAL END
      PROGRAM SETFC
C-----------------------------------------------------------------------
C! do make a BOXFILE for input to IMAGR
C# Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 1999-2009, 2011-2012, 2015
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
C   SETFC makes a file which can then be used as an input BOXFILE
C   for IMAGR.  This is intended for wide-field imaging nd to cope with
C   interfering sources including the Sun.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME       XNAMEI/NAMEIN    Name of input UV data.
C      INCLASS      XCLASI/CLASIN    Class of input UV data.
C      INSEQ        SEQ/SEQIN        Seq. of input UV data.
C      INDISK       DISKIN/IDISK     Disk number of input UV data.
C      SOURCES      XXSOUR/SOURCE    Source which will be mapped.
C      BOXFILE      XBXFIL/BOXFIL    BOXFILE output file name.
C      CELLSIZE     CELSIZ           pixel size in image.
C      IMSIZE       XSIZE/IMSIZE     image size, also field size.
C      SHIFT        SHIFT            offset for all fields.
C      BPARM        BPARM            1. Fly-eye radius (deg)
C                                    2. Overlap in fly-eye
C                                    3. Catalog radius
C                                    4. Catalog flux
C                                    5. cat image size
C      INFILE       XINFIL/NVSSF     NVSS input file name.
C   programmer: bryan butler, spring 1999.  copied idea from Lazio &
C   Kassim which is implemented in SETFAC (a RUN file).  this one is for
C   > 64 NFIELD, when you will need to make a BOXFILE for input to
C   IMAGR.  i added the capability to add in outlier fields for strong
C   NVSS sources, and the Sun.
C   Revised to AIPS standards by ewg.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PNAM*6, MTYPE*2, STAT*4, OBSDAT*8
      INTEGER   NPARMS, IRET, IERR, IROUND, IBUFF(512), LUNSU, SQUAL,
     *   SOUID, NID, OBSDAY(6), FREQAX, FLDSIZ, IFIELD, IVER
      REAL      RADIUS, START, STOP, CATR(256), AFLUX, RETPRM(5)
      DOUBLE PRECISION LAMBD, CATD(128), DTEMP
      HOLLERITH CATH(256)
      LOGICAL   APPEND, BXOPEN, NVOPEN, STD, MULTI
      INCLUDE 'SETFC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATH, CATD, CATR, CATBLK)
      DATA NPARMS, PNAM /177, 'SETFC '/
      DATA LUNSU /27/
      DATA NID, SQUAL /1, -1/
      DATA MTYPE /'UV'/
C-----------------------------------------------------------------------
C                                       initialize
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       get the input parameters
      CALL GTPARM (PNAM, NPARMS, RQUICK, XNAMEI, IBUFF, IERR)
      RQUICK = .FALSE.
      IF (IERR.NE.0) THEN
         IRET = 1
         IF (IERR.EQ.1) THEN
            MSGTXT = 'CANNOT FIND INITIATOR IN GTPARM'
            CALL MSGWRT (1)
            GO TO 999
         ELSE
            MSGTXT= 'DISK PROBLEMS IN GTPARM'
            CALL MSGWRT (6)
            GO TO 995
            END IF
         END IF
      IRET = 0
C                                       set parameters
      LUNBX = 10
      LUNNV = 11
      BXOPEN = .FALSE.
      NVOPEN = .FALSE.
      IMSIZE(1) = IROUND (XSIZE(1))
      IMSIZE(2) = IROUND (XSIZE(2))
      OVRLAP = IROUND (BPARM(2))
      IF (OVRLAP.LT.1) OVRLAP = 5
      BPARM(2) = OVRLAP
      IF (BPARM(3).LE.0.0) BPARM(3) = 1.0
      OFIELD = XBCNT + 0.1
      OFIELD = MAX (1, OFIELD) - 1
      XBCNT = OFIELD
      IFIELD = OFIELD + 1
      WRBOX = IROUND (BPARM(8))
      IF (WRBOX.GT.0) WRBOX = MAX (5, WRBOX)
      CALL H2CHR (48, 1, XBXFIL, BOXFIL)
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      CALL H2CHR (48, 1, XINFIL, NVSSF)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      SEQIN = IROUND (XSEQ)
      IDISK = IROUND (DISKIN)
C                                       locate map in directory
      CATNO = 1
      CALL CATDIR ('SRCH', IDISK, CATNO, NAMEIN, CLASIN, SEQIN,
     *   MTYPE, NLUSER, STAT, IBUFF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'REQUESTED DATA NOT FOUND IN CATALOG DIRECTORY'
         CALL MSGWRT (6)
         GO TO 995
         END IF
C                                       read catalog block
      CALL CATIO ('READ', IDISK, CATNO, CATBLK, 'REST', IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 995
         END IF
      WRITE (MSGTXT,1010) NAMEIN, CLASIN, SEQIN, IDISK, CATNO
      CALL MSGWRT (3)
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 995
      UNAME = NAMEIN
      UCLAS = CLASIN
      UDISK = IDISK
      USEQ = SEQIN
C                                       get antenna info
      IVER = 1
      CALL ANTINI ('READ', ANBUFF, IDISK, CATNO, IVER, CATBLK, LUNSU,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.EQ.0) CALL TABIO ('CLOS', 0, 1, ANBUFF, ANBUFF, IERR)
      IF (IERR.EQ.0) CALL GETANT (IDISK, CATNO, IVER, CATBLK, IBUFF,
     *   IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'UNABLE TO GET ANTENNA INFORMATION'
         GO TO 980
         END IF
C                                       look for an SU table
      CALL MULSDB (CATBLK, MULTI)
C                                       single-source use UVPGET output
      IF (.NOT.MULTI) THEN
         SOUID = -1
         RA0 = RA
         DEC0 = DEC
         IF ((RA0.EQ.0.0D0) .AND. (DEC0.EQ.0.0D0)) THEN
            MSGTXT = 'CANNOT FIND SOURCE POSITION'
            GO TO 980
            END IF
C                                       multi-source get 1 source
      ELSE
         CALL H2CHR (16, 1, XXSOUR(1,1), SOURCS(1))
C                                       multi-source file, but no source
C                                       specified...
         IF (SOURCS(1).EQ.' ') THEN
            MSGTXT = 'MULTI SOURCE FILE BUT NO SOURCE SPECIFIED!'
            GO TO 980
C                                       find source...
         ELSE
            CALL SOURNU (SOURCS(1), SQUAL, 1, IDISK, CATNO, NID, IBUFF,
     *         SOUID, IERR)
            IF (IERR.EQ.0) THEN
               CALL GETSOU (SOUID, IDISK, CATNO, CATBLK, LUNSU, IERR)
               IF (IERR.EQ.0) THEN
                  DEC0 = DECEPO * RAD2DG
                  RA0 = RAEPO * RAD2DG
               ELSE
                  MSGTXT = 'ERROR ACCESSING SU EXTENSION'
                  GO TO 980
                  END IF
            ELSE
               MSGTXT = 'ERROR FINDING REQUESTED SOURCE'
               GO TO 980
               END IF
            END IF
         END IF
C                                       Get times
      CALL SETIME (IDISK, CATNO, SOUID, START, STOP, IERR)
C                                       Get CELSIZ, IMSIZE
      RADIUS = BPARM(1)
      IF ((CELSIZ(1).LE.0.0) .OR. (CELSIZ(2).LE.0.0) .OR.
     *   (IMSIZE(1).LE.0) .OR. (IMSIZE(2).LE.0)) THEN
         CALL SETCEL (DEC0, RADIUS, BPARM(9), IMSIZE, CELSIZ, IERR)
         IF (IERR.NE.0) GO TO 995
         XSIZE(1) = IMSIZE(1)
         XSIZE(2) = IMSIZE(2)
         END IF
C                                       Open output
      APPEND = .TRUE.
      CALL ZTXOPN ('WRIT', LUNBX, INDBX, BOXFIL, APPEND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 980
         END IF
      BXOPEN = .TRUE.
C                                       Compute fly's eye and write
      CALL FLYEYE (RADIUS, OFIELD, IMSIZE, CELSIZ, OVRLAP, SHIFT, RA0,
     *   DEC0, LUNBX, INDBX, OPTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Do catalog search
      IF (BPARM(4).GT.ABS(RADIUS)) THEN
         WRITE (MSGTXT,1110) RADIUS, BPARM(4)
         CALL MSGWRT (3)
C                                       Open catalog file
         IF (NVSSF.EQ.' ') THEN
            IF (BPARM(5).GE.1.0) THEN
               NVSSF = 'AIPSTARS:NV00.1000'
            ELSE IF (BPARM(5).GE.0.300) THEN
               NVSSF = 'AIPSTARS:NV00.0300'
            ELSE IF (BPARM(5).GE.0.100) THEN
               NVSSF = 'AIPSTARS:NV00.0100'
            ELSE
               NVSSF = 'AIPSTARS:NV00.0030'
               END IF
            IF (ABS(CATR(KREPO)-1950.0).LT.0.1) NVSSF(12:13) = '50'
            END IF
         STD = NVSSF(:8).EQ.'AIPSTARS'
         IF (DEC0+RADIUS.GT.90.0D0) STD = .FALSE.
         IF (DEC0-RADIUS.LT.-90.0D0) STD = .FALSE.
         IF (STD) THEN
            DTEMP = RADIUS / COS (DG2RAD * DEC0)
            IF (RA0-DTEMP.LT.0.0D0) STD = .FALSE.
            IF (RA0+DTEMP.GT.360.0D0) STD = .FALSE.
            END IF
         CALL ZTXOPN ('READ', LUNNV, INDNV, NVSSF, .FALSE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1200) IERR
            GO TO 980
            END IF
         NVOPEN = .TRUE.
C                                       find frequency
         CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATBLK(KHCTP), FREQAX,
     *      IERR)
         IF (IERR.EQ.0) THEN
            FREQ = CATD(KDCRV+FREQAX)
            LAMBD = VELITE / FREQ
         ELSE
            LAMBD = 0.0D0
            END IF
C                                       write external fields
         FLDSIZ = BPARM(6) + 0.1
         IF (FLDSIZ.LE.32) FLDSIZ = 128
         FLDSIZ = FLDSIZ - 10
         AFLUX = BFLUX / BPARM(3)
         CALL ADNVSS (BPARM(5), AFLUX, RADIUS, CELSIZ, FLDSIZ, BPARM(4),
     *      RA0, DEC0, LAMBD, OFIELD, LUNNV, INDNV, LUNBX, INDBX,
     *      PBPARM, WRBOX, STD, OPTYPE, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       do Sun for now
         CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
         READ (OBSDAT,1020) OBSDAY(1), OBSDAY(2), OBSDAY(3)
         START = (START + STOP) * 12.0
         OBSDAY(4) = START
         START = 60.0 * (START - OBSDAY(4))
         OBSDAY(5) = START
         START = 60.0 * (START - OBSDAY(5))
         OBSDAY(6) = IROUND (START)
         FLDSIZ = BPARM(7) + 0.1
         IF (FLDSIZ.LE.32) THEN
            FLDSIZ = 1.333 * 1800.0 / CELSIZ(1)
            IF (FLDSIZ.GT.1024) THEN
               FLDSIZ = 2048
            ELSE IF (FLDSIZ.GT.512) THEN
               FLDSIZ = 1024
            ELSE IF (FLDSIZ.GT.256) THEN
               FLDSIZ = 512
            ELSE IF (FLDSIZ.GT.128) THEN
               FLDSIZ = 256
            ELSE
               FLDSIZ = 128
               END IF
            END IF
         FLDSIZ = FLDSIZ - 10
         CALL ADDSUN (OBSDAY, RADIUS, CELSIZ, FLDSIZ, BPARM(4), RA0,
     *      DEC0, OFIELD, LUNBX, INDBX, WRBOX, OPTYPE, IERR)
         END IF
      WRITE (MSGTXT,1050) 'First', IFIELD
      CALL MSGWRT (4)
      WRITE (MSGTXT,1050) 'Highest', OFIELD
      CALL MSGWRT (4)
      GO TO 990
C                                       Error
 980  CALL MSGWRT (8)
C                                       close files and go to bed.
 990  IRET = IERR
      IF (NVOPEN) THEN
         CALL ZTXCLS (LUNNV, INDNV, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1990) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      IF (BXOPEN) THEN
         CALL ZTXCLS (LUNBX, INDBX, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1991) IERR
            CALL MSGWRT (6)
            END IF
         END IF
C                                       close files and go to bed.
 995  IF (.NOT.RQUICK) THEN
         RETPRM(1) = CELSIZ(1)
         RETPRM(2) = CELSIZ(2)
         RETPRM(3) = XSIZE(1)
         RETPRM(4) = XSIZE(2)
         RETPRM(5) = OFIELD
         CALL PTPARM (5, RETPRM, IBUFF, IERR)
         END IF
      CALL DIETSK (IRET, RQUICK, IBUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('CATIO ERROR NO', I5)
 1010 FORMAT ('Found ',A12,'.',A6,'.',I5.5,' Disk',I3,' slot',I6)
 1020 FORMAT (I4,2I2)
 1050 FORMAT (A,' field number written was',I4)
 1100 FORMAT ('ERROR NO', I5, ' OPENING OUTPUT TEXT FILE')
 1110 FORMAT ('Searching catalog between',F6.2,' and ',F6.2,
     *   ' degrees radius')
 1200 FORMAT ('ERROR NO', I5, ' OPENING NVSS FILE')
 1990 FORMAT ('ERROR NO', I5, ' CLOSING NVSS FILE')
 1991 FORMAT ('ERROR NO', I5, ' CLOSING OUTPUT TEXT FILE')
      END
      SUBROUTINE FLYEYE (RADIUS, OFIELD, IMSIZE, PIXSIZ, OVRLAP, SHIFT,
     *   RA0, DEC0, LUNBX, INDBX, OPTYPE, IERR)
C-----------------------------------------------------------------------
C   Writes fly's eye portion of BOXFILE
C   Inputs:
C      IMSIZE   I(2)   Output image size
C      PIXSIZ   R(2)   Pixel size (asec)
C      OVRLAP   I      Number pixels of overlap
C      SHIFT    R(2)   Shift all coordinates
C      RA0      D      Center RA in degrees
C      DEC0     D      Center Dec in degrees
C      LUNBX    I      LUN of Box file
C      INDBX    I      FTAB pointer of box file
C   In/Out:
C      RADIUS   R      In: Radius of fly's eye in degree
C                          if < 0 simply compute, do not write
C                      Out: Radius of zone of avoidence for externals
C      OFIELD   I      Number fields already written
C   Output:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      DOUBLE PRECISION RA0, DEC0
      REAL      RADIUS, PIXSIZ(2), SHIFT(2)
      INTEGER   OFIELD, IMSIZE(2), OVRLAP, LUNBX, INDBX, IERR
      CHARACTER OPTYPE*4
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXF
      PARAMETER (MAXF=6*MAXFLD)
C
      DOUBLE PRECISION COSDEC, SINDEC, COSFLD, SINFLD, RASEC, DECSEC,
     *   RA(MAXF), DEC(MAXF), RA2, DEC2, DX, DY, XRA, XDEC, XD,
     *   XSH(2), LL, MM, RA0R, DEC0R, DRADD, DRAD
      REAL      DIST(MAXF)
      LOGICAL   THIS1
      INTEGER   IFIELD, MFIELD, II, JJ, INDX(MAXF), RAHR, RAMIN,
     *   DECDEG, DECMIN, I1, I2, JERR, JFIELD, I3, I4, IT, JTRIM
      CHARACTER OUTLIN*48, DECS*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL RFILL (MAXF, -1.0, DIST)
      COSDEC = COS (DEC0*DG2RAD)
      SINDEC = SIN (DEC0*DG2RAD)
      RA0R = RA0 * DG2RAD
      DEC0R = DEC0 * DG2RAD
      XSH(2) = SHIFT(2) / 3.6D3
      XSH(1) = SHIFT(1) / 3.6D3 / COSDEC
C                                       Spacing of imaging region (rad)
      DRAD = 0.5 * AS2RAD * PIXSIZ(1) * (IMSIZE(1) - 11 - OVRLAP)
      DX = 1.5 * DRAD
      DY = COS (30.0 * DG2RAD) * DRAD
C                                       field size in degrees
      DRADD = DRAD * RAD2DG / SQRT (2.0)
C                                       collect list
      IFIELD = 0
      MFIELD = 2.0 * SQRT (MAXF / PI)
      I2 = MFIELD/2 - 3
      I4 = I2 * (DX/DY) + 0.5
      I4 = (I4 / 2) * 2 - 1
      IF (MOD((2*I2+1)*I4+I2,2).EQ.1) I2 = I2 + 1
      I1 = -I2
      I3 = -I4
      THIS1 = .FALSE.
C                                       loop over declination
      DO 20 II = I3,I4
         MM = II * DY
C                                       loop over RA
         DO 10 JJ = I1,I2
            LL = JJ * DX
C                                       do every other possibility to
C                                       get hexagonal pattern
            THIS1 = .NOT.THIS1
            IF (THIS1) THEN
C                                       -> RA, Dec (-SIN projection)
               MSGSUP = 32000
               CALL NEWPOS (2, RA0R, DEC0R, LL, MM, XRA, XDEC, JERR)
               MSGSUP = 0
C                                       Okay - convert to degrees
               IF (JERR.EQ.0) THEN
                  XRA = XRA * RAD2DG + XSH(1)
                  XDEC = XDEC * RAD2DG + XSH(2)
                  COSFLD = COS (XDEC*DG2RAD)
                  SINFLD = SIN (XDEC*DG2RAD)
                  XD = SINFLD * SINDEC +
     *               COSFLD * COSDEC * COS (XRA*DG2RAD - RA0R)
                  IF (XD.GT.1.0D0) THEN
                     XD = 0.0D0
                  ELSE
                     XD = RAD2DG * ACOS (XD)
                     END IF
                  IF (XD.LT.(ABS(RADIUS)+DRADD)) THEN
                     IF (IFIELD.LT.MAXF) THEN
                        IFIELD = IFIELD + 1
                        RA(IFIELD) = XRA
                        DEC(IFIELD) = XDEC
                        DIST(IFIELD) = XD
                        INDX(IFIELD) = IFIELD
                     ELSE
                        MSGTXT = 'FLYEYE: NUMBER OF FIELDS EXCEEDED'
                        CALL MSGWRT (7)
                        IERR = 1
                        GO TO 999
                        END IF
                     END IF
                  END IF
               END IF
 10         CONTINUE
 20      CONTINUE
      MFIELD = IFIELD
C                                       sort on distances
      CALL BUBBLE (DIST, INDX, IFIELD, 1)
C                                       write out
      IF (RADIUS.GT.0.0) THEN
         DO 30 II = 1,MFIELD
            IF (OFIELD.LT.MAXFLD) THEN
               JFIELD = II
               OFIELD = OFIELD + 1
C                                       Field center card
               IF (OPTYPE.NE.'USEF') THEN
                  RA2 = RA(INDX(II)) / 15.0D0
                  DEC2 = DEC(INDX(II))
                  IF (RA2.LT.0.0D0) RA2 = RA2 + 24.0D0
                  IF (RA2.GT.24.0D0) RA2 = RA2 - 24.0D0
                  IF (DEC2.LT.0.0D0) THEN
                     DECS = '-'
                     DEC2 = ABS (DEC2)
                  ELSE
                     DECS = ' '
                     END IF
                  CALL DTRANS (DEC2, DECDEG, DECMIN, DECSEC)
                  CALL DTRANS (RA2, RAHR, RAMIN, RASEC)
                  I1 = IMSIZE(1) - 12
                  I2 = IMSIZE(2) - 12
                  WRITE (OUTLIN,1020) OFIELD, I1, I2, RAHR, RAMIN,
     *               RASEC, DECS, DECDEG, DECMIN, DECSEC
                  IF (OUTLIN(29:29).EQ.' ') OUTLIN(29:29) = '0'
                  IF (OUTLIN(43:43).EQ.' ') OUTLIN(43:43) = '0'
               ELSE
                  RA2 = (RA(INDX(II)) - RA0)
                  IF (RA2.GT.180.0D0) RA2 = RA2 - 360.0D0
                  IF (RA2.LT.-180.0D0) RA2 = RA2 + 360.0D0
                  RA2 = RA2 * 3600.0D0 * COS (DEC0*DG2RAD)
                  DEC2 = (DEC(INDX(II)) - DEC0) * 3600.0
                  WRITE (OUTLIN,1021) OFIELD, I1, I2, RA2, DEC2
                  END IF
               IT = JTRIM (OUTLIN)
               CALL ZTXIO ('WRIT', LUNBX, INDBX, OUTLIN(:IT), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1022) IERR
                  CALL MSGWRT (6)
                  OFIELD = OFIELD - 1
                  GO TO 999
                  END IF
C                                       Clean box (circle)
               JJ = MIN (IMSIZE(1), IMSIZE(2)) / 2 - 5
               I1 = IMSIZE(1) / 2
               I2 = IMSIZE(2)/2 + 1
               WRITE (OUTLIN,1025) OFIELD, JJ, I1, I2
               IT = JTRIM (OUTLIN)
               CALL ZTXIO ('WRIT', LUNBX, INDBX, OUTLIN(:IT), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1022) IERR
                  CALL MSGWRT (6)
                  GO TO 999
                  END IF
               END IF
 30         CONTINUE
         WRITE (MSGTXT,1030) JFIELD, DIST(JFIELD)
         CALL MSGWRT (4)
         IF (DIST(JFIELD).LT.0.95*RADIUS) THEN
            MSGTXT = 'WARNING: THIS MAY NOT COVER THE DESIRED AREA'
            CALL MSGWRT (7)
            END IF
         END IF
C
      RADIUS = DIST(MFIELD)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('C ',I5,2I6,2X,2I3.2,F6.2,2X,A1,I2.2,I3.2,F5.1)
 1021 FORMAT ('F ',I5,2I6,2X,2F13.4)
 1022 FORMAT ('ERROR NO',I5,' WRITING BOXFILE IN FLYEYE')
 1025 FORMAT (I5.5,'  -1',3I7)
 1030 FORMAT ('FLYEYE added',I4,' fields to BOXFILE to',F7.3,' deg')
      END
      SUBROUTINE ADNVSS (MINFLX, BFLUX, RADIUS, CELSIZ, FLDSIZ, MAXANG,
     *   RA0, DEC0, LAMBDA, OFIELD, LUNNV, INDNV, LUNBX, INDBX, PBPARM,
     *   WRBOX, STD, OPTYPE, IERR)
C-----------------------------------------------------------------------
C   Searches the catalog for sources in the desired RA, Dec, and Flux
C   range taking into account the estimated single-dish beam
C   Inputs:
C      MINFLX   R   Minimum flux in Jy
C      BFLUX    R   Minimum flux * beam in Jy
C      RADIUS   R   Do not include sources within RADIUS deg
C      CELSIZ   R   Cell size in arc sec
C      FLDSIZ   I   Field size to use
C      MAXANG   R   Maximum radius (deg) for catalog sources
C      RA0      D   RA of center (degrees)
C      DEC0     D   Dec of center (degrees)
C      LAMBDA   D   Wavelength
C      LUNNV    I   LUN of catalog
C      INDNV    I   FTAB pointer of catalog
C      LUNBX    I   LUN of Box file
C      INDBX    I   FTAB pointer of Box file
C      WRBOX    I   Write Clean boxes for sources
C      STD      L   Standard input file
C   In/out:
C      OFIELD   I   Number of fields already written
C   Output
C      IERR     I   Error code
C-----------------------------------------------------------------------
      DOUBLE PRECISION RA0, DEC0, LAMBDA
      REAL      MINFLX, BFLUX, RADIUS, CELSIZ(2), MAXANG, PBPARM(7)
      INTEGER   FLDSIZ, OFIELD, LUNNV, INDNV, LUNBX, INDBX, WRBOX, IERR
      LOGICAL   STD
      CHARACTER OPTYPE*4
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXF
      PARAMETER (MAXF = MAXFLD*3+1)
      DOUBLE PRECISION CALRA(MAXF), CALDEC(MAXF), RASEC, DECSEC, SINDEC,
     *   COSDEC, SINCAL, COSCAL, SRA, TSEP, TSIN, TCOS, XRA, XDEC, XD,
     *   CLLRA(MAXF,50), CLLDEC(MAXF,50), DX, DY, XMIN, XMAX
      REAL      FLYADD, SEPN(MAXF), BMFACT(MAXF), CALFLX(MAXF),
     *   CLFLX2(MAXF), XF, SEPMX(MAXF), SEPMN(MAXF)
      INTEGER   RAHR, RAMIN, DECDEG, DECMIN, NCAT, INDX(MAXF), II, JJ,
     *   LFLUX, NWRIT, NSORC(MAXF), LFLD, NACTW, NABOVE, FLDS(MAXF), J,
     *   I1, I2, I3, IT, JTRIM, NMAX, NN, I
      LOGICAL   FOUND, OUTSID, LSTD, DONE
      CHARACTER INLINE(25)*132, OUTLIN*48, DECS*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANT.INC'
C-----------------------------------------------------------------------
      FLYADD = FLDSIZ * CELSIZ(1) / 3600.
      SINDEC = SIN (DEC0*DG2RAD)
      COSDEC = COS (DEC0*DG2RAD)
      SRA = RA0 * DG2RAD
      NCAT = 0
      NACTW = 0
      BFLUX = BFLUX / 3.0
      DONE = .FALSE.
      LSTD = STD
      IF (STD) THEN
         XD = 1.2 * MAXANG / COSDEC
         XMIN = RA0 - XD
         XMAX = RA0 + XD
         LSTD = (XMIN.GT.0.0) .AND. (XMAX.LT.360.0)
         END IF
      IF (LSTD) THEN
         NMAX = 25
      ELSE
         NMAX = 1
         XMAX = 1.D4
         END IF
C                                       loop in file
 10   NN = 1
 11   CALL ZTXIO ('READ', LUNNV, INDNV, INLINE(NN), IERR)
      IF (IERR.EQ.2) THEN
         NMAX = NN
         IERR = 0
         DONE = .TRUE.
         GO TO 15
      ELSE IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (6)
         GO TO 999
      ELSE
         I = JTRIM (INLINE(NN))
         IF (INLINE(NN)(1:1).EQ.';') GO TO 11
         NN = NN + 1
         IF (NN.LE.NMAX) GO TO 11
         NN = NMAX
         END IF
C                                       finished a buffer
 15   IF (NMAX.GT.1) THEN
         READ (INLINE(NMAX),1010) XRA
         IF (XRA.LT.XMIN) GO TO 10
         NMAX = 1
         END IF
C                                       empty buffer
      DO 26 I = 1,NN
         READ (INLINE(I),1010) XRA, XDEC, LFLUX
         IF (XRA.GT.XMAX) GO TO 30
         XF = LFLUX / 1000.0
         IF ((XF.LT.MINFLX) .OR. (XF.LT.BFLUX)) GO TO 26
         XDEC = XDEC * DG2RAD
         XRA = XRA * DG2RAD
         SINCAL = SIN (XDEC)
         COSCAL = COS (XDEC)
         XD = SINCAL * SINDEC + COSCAL * COSDEC * COS(XRA - SRA)
         IF (XD.GT.1.0D0) THEN
            XD = 0.0D0
         ELSE
            XD = RAD2DG * ACOS (XD)
            END IF
         IF ((XD.LT.MAXANG) .AND. (XD.GT.RADIUS)) THEN
            CALDEC(NCAT+1) = XDEC
            CALRA(NCAT+1) = XRA
            SEPN(NCAT+1) = XD
            CALFLX(NCAT+1) = XF
            CALL PBCALC (XD, LAMBDA, ANAME, PBPARM(2), BMFACT(NCAT+1),
     *         OUTSID)
            IF (OUTSID) BMFACT(NCAT+1) = 0.01
            CLFLX2(NCAT+1) = XF * BMFACT(NCAT+1)
            IF ((CLFLX2(NCAT+1).GE.BFLUX) .AND.
     *         (BMFACT(NCAT+1).GE.PBPARM(1))) THEN
C                                       check the separation of
C                                       previously found sources.
               FOUND = .FALSE.
               DO 20 II = 1,NCAT
                  TSIN = SIN (CALDEC(II)/NSORC(II))
                  TCOS = COS (CALDEC(II)/NSORC(II))
                  TSEP = SINCAL * TSIN +
     *               COSCAL * TCOS * COS(XRA - CALRA(II)/NSORC(II))
                  IF (TSEP.GT.1.0D0) THEN
                     TSEP = 0.0D0
                  ELSE
                     TSEP = RAD2DG * ACOS (TSEP)
                     END IF
C                                       change to the new source if it
C                                       is stronger...
                  IF (TSEP.LT.FLYADD/3.) THEN
                     CALRA(II) = CALRA(II) + CALRA(NCAT+1)
                     CALDEC(II) = CALDEC(II) + CALDEC(NCAT+1)
                     CALFLX(II) = CALFLX(II) + CALFLX(NCAT+1)
                     SEPN(II) = SEPN(II) + SEPN(NCAT+1)
                     BMFACT(II) = BMFACT(II) + BMFACT(NCAT+1)
                     CLFLX2(II) = CLFLX2(II) + CLFLX2(NCAT+1)
                     SEPMX(II) = MAX (SEPMX(II), SEPN(NCAT+1))
                     SEPMN(II) = MIN (SEPMN(II), SEPN(NCAT+1))
                     NSORC(II) = NSORC(II) + 1
                     IF (NSORC(II).LE.50) THEN
                        CLLRA(II,NSORC(II)) = CALRA(NCAT+1)
                        CLLDEC(II,NSORC(II)) = CALDEC(NCAT+1)
                        END IF
                     FOUND = .TRUE.
                     GO TO 25
                     END IF
 20               CONTINUE
 25            IF (.NOT.FOUND) THEN
                  NCAT = NCAT + 1
                  INDX(NCAT) = NCAT
                  SEPMX(NCAT) = XD
                  SEPMN(NCAT) = XD
                  NSORC(NCAT) = 1
                  CLLRA(NCAT,1) = CALRA(NCAT)
                  CLLDEC(NCAT,1) = CALDEC(NCAT)
                  IF (NCAT.EQ.MAXF) THEN
                     MSGTXT = 'SOURCE TABLE FULL: RAISE MIN FLUX'
                     CALL MSGWRT (8)
                     IERR = 4
                     GO TO 999
                     END IF
                  END IF
               END IF
            END IF
 26      CONTINUE
      IF (.NOT.DONE) GO TO 10
C                                       Scale
 30   IERR = 0
      BFLUX = BFLUX * 3.0
      NABOVE = 0
      DO 35 II = 1,NCAT
         IF (NSORC(II).GT.1) THEN
            CALRA(II) = CALRA(II) / NSORC(II)
            CALDEC(II) = CALDEC(II) / NSORC(II)
            SEPN(II) = SEPN(II) / NSORC(II)
            BMFACT(II) = BMFACT(II) / NSORC(II)
            IF ((SEPMX(II)-SEPN(II).GT.FLYADD/6.) .OR.
     *         (SEPN(II)-SEPMN(II).GT.FLYADD/6.)) THEN
               FLDS(II) = FLDSIZ * (1. + (SEPMX(II)-SEPMN(II))/FLYADD)
            ELSE
               FLDS(II) = FLDSIZ
               END IF
         ELSE
            FLDS(II) = FLDSIZ
            IF (CLFLX2(II).GE.BFLUX) NABOVE = NABOVE + 1
            END IF
 35         CONTINUE
C                                       All read in
      IF (NCAT.LE.0) THEN
         MSGTXT = 'No NVSS sources selected for BOXFILE'
         CALL MSGWRT (3)
      ELSE
         WRITE (MSGTXT,1030)
         CALL MSGWRT (3)
C                                       All fit
         IF (OFIELD+NABOVE.LT.MAXFLD) THEN
C                                       sort on separation distance...
            CALL BUBBLE (SEPN, INDX, NCAT, 1)
            DO 50 II = 1,NCAT
               JJ = INDX(II)
               IF (CLFLX2(JJ).GE.BFLUX) THEN
                  XRA = CALRA(JJ) * RAD2DG / 15.0D0
                  XDEC = CALDEC(JJ) * RAD2DG
                  IF (XRA.LT.0.0D0) XRA = XRA + 24.0D0
                  IF (XDEC.LT.0.0D0) THEN
                     DECS = '-'
                     XDEC = ABS (XDEC)
                  ELSE
                     DECS = ' '
                     END IF
                  CALL DTRANS (XDEC, DECDEG, DECMIN, DECSEC)
                  CALL DTRANS (XRA, RAHR, RAMIN, RASEC)
C                                       message to user
                  OFIELD = OFIELD + 1
                  XF = CALFLX(JJ)
                  WRITE (MSGTXT,1031) OFIELD, RAHR, RAMIN, RASEC,
     *               DECS, DECDEG, DECMIN, DECSEC, XF, BMFACT(JJ),
     *               SEPN(II)
                  IF (MSGTXT(16:16).EQ.' ') MSGTXT(16:16) = '0'
                  IF (MSGTXT(31:31).EQ.' ') MSGTXT(31:31) = '0'
                  CALL MSGWRT (3)
C                                       field center card
                  IF (OPTYPE.NE.'USEF') THEN
                     LFLD = FLDS(JJ)
                     WRITE (OUTLIN,1035) OFIELD, LFLD, LFLD, RAHR,
     *                  RAMIN, RASEC, DECS, DECDEG, DECMIN, DECSEC
                     IF (OUTLIN(29:29).EQ.' ') OUTLIN(29:29) = '0'
                     IF (OUTLIN(43:43).EQ.' ') OUTLIN(43:43) = '0'
                  ELSE
                     XRA = (CALRA(JJ)*RAD2DG - RA0)
                     IF (XRA.GT.180.0D0) XRA = XRA - 360.0D0
                     IF (XRA.LT.-180.0D0) XRA = XRA + 360.0D0
                     XRA = XRA * 3600.0D0 * COS (DEC0*DG2RAD)
                     XDEC = (CALDEC(JJ)*RAD2DG - DEC0) * 3600.0
                     WRITE (OUTLIN,1036) OFIELD, I1, I2, XRA, XDEC
                     END IF
                  IT = JTRIM (OUTLIN)
                  NACTW = NACTW + 1
                  CALL ZTXIO ('WRIT', LUNBX, INDBX, OUTLIN(:IT), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1037) IERR
                     CALL MSGWRT (6)
                     GO TO 999
                     END IF
C                                       Clean circles
                  IF (WRBOX.GT.0) THEN
                     COSCAL = COS (CALDEC(JJ))
                     CALL POWER2 (LFLD, LFLD)
                     DO 40 J = 1,NSORC(JJ)
                        IF (J.LE.50) THEN
                           DX = (CLLRA(JJ,J) - CALRA(JJ)) * COSCAL *
     *                        RAD2AS / CELSIZ(1)
                           DY = (CLLDEC(JJ,J) - CALDEC(JJ)) * RAD2AS /
     *                        CELSIZ(2)
                           I1 = LFLD/2 - DX + 0.5
                           I2 = LFLD/2 + DY + 1.5
                           I3 = MIN (LFLD/2-6, WRBOX)
                           WRITE (OUTLIN,1040) OFIELD, I3, I1, I2
                           IT = JTRIM (OUTLIN)
                           CALL ZTXIO ('WRIT', LUNBX, INDBX,
     *                        OUTLIN(:IT), IERR)
                           IF (IERR.NE.0) THEN
                              WRITE (MSGTXT,1037) IERR
                              CALL MSGWRT (6)
                              GO TO 999
                              END IF
                           END IF
 40                     CONTINUE
                     END IF
                  END IF
 50            CONTINUE
            WRITE (MSGTXT,1050) NACTW
            CALL MSGWRT (4)
C                                       Too many: take strongest
         ELSE
            NWRIT = MAXFLD - 1 - OFIELD
            CALL BUBBLE (CLFLX2, INDX, NCAT, -1)
            DO 70 II = 1,NWRIT
               JJ = INDX(II)
               IF (CLFLX2(II).GE.BFLUX) THEN
                  XRA = CALRA(JJ) * RAD2DG / 15.0D0
                  XDEC = CALDEC(JJ) * RAD2DG
                  IF (XRA.LT.0.0D0) XRA = XRA + 24.0D0
                  IF (XDEC.LT.0.0D0) THEN
                     DECS = '-'
                     XDEC = ABS (XDEC)
                  ELSE
                     DECS = ' '
                  END IF
                  CALL DTRANS (XDEC, DECDEG, DECMIN, DECSEC)
                  CALL DTRANS (XRA, RAHR, RAMIN, RASEC)
C                                       message to user
                  OFIELD = OFIELD + 1
                  XF = CALFLX(JJ)
                  IF (MSGTXT(16:16).EQ.' ') MSGTXT(16:16) = '0'
                  IF (MSGTXT(31:31).EQ.' ') MSGTXT(31:31) = '0'
                  WRITE (MSGTXT,1031) OFIELD, RAHR, RAMIN, RASEC, DECS,
     *               DECDEG, DECMIN, DECSEC, XF, BMFACT(JJ),
     *               SEPN(JJ)
                  CALL MSGWRT (3)
C                                       Field center card
                  LFLD = FLDS(JJ)
                  IF (OPTYPE.NE.'USEF') THEN
                     WRITE (OUTLIN,1035) OFIELD, LFLD, LFLD, RAHR,
     *                  RAMIN, RASEC, DECS, DECDEG, DECMIN, DECSEC
                     IF (OUTLIN(29:29).EQ.' ') OUTLIN(29:29) = '0'
                     IF (OUTLIN(43:43).EQ.' ') OUTLIN(43:43) = '0'
                  ELSE
                     XRA = (CALRA(JJ)*RAD2DG - RA0)
                     IF (XRA.GT.180.0D0) XRA = XRA - 360.0D0
                     IF (XRA.LT.-180.0D0) XRA = XRA + 360.0D0
                     XRA = XRA * 3600.0D0 * COS (DEC0*DG2RAD)
                     XDEC = (CALDEC(JJ)*RAD2DG - DEC0) * 3600.0
                     WRITE (OUTLIN,1036) OFIELD, I1, I2, XRA, XDEC
                     END IF
                  IT = JTRIM (OUTLIN)
                  NACTW = NACTW + 1
                  CALL ZTXIO ('WRIT', LUNBX, INDBX, OUTLIN(:IT), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1037) IERR
                     CALL MSGWRT (6)
                     GO TO 999
                     END IF
C                                       Clean circles
                  IF (WRBOX.GT.0) THEN
                     COSCAL = COS (CALDEC(JJ))
                     CALL POWER2 (LFLD, LFLD)
                     DO 60 J = 1,NSORC(JJ)
                        IF (J.LE.50) THEN
                           DX = (CLLRA(JJ,J) - CALRA(JJ)) * COSCAL *
     *                        RAD2AS / CELSIZ(1)
                           DY = (CLLDEC(JJ,J) - CALDEC(JJ)) * RAD2AS /
     *                        CELSIZ(2)
                           I1 = LFLD/2 - DX + 0.5
                           I2 = LFLD/2 + DY + 1.5
                           I3 = MIN (LFLD/2-6, WRBOX)
                           WRITE (OUTLIN,1040) OFIELD, I3, I1, I2
                           IT = JTRIM (OUTLIN)
                           CALL ZTXIO ('WRIT', LUNBX, INDBX,
     *                        OUTLIN(:IT), IERR)
                           IF (IERR.NE.0) THEN
                              WRITE (MSGTXT,1037) IERR
                              CALL MSGWRT (6)
                              GO TO 999
                              END IF
                           END IF
 60                     CONTINUE
                     END IF
                  END IF
 70            CONTINUE
            WRITE (MSGTXT,1050) NACTW
            CALL MSGWRT (4)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (F9.5,1X,F9.5,I7)
 1020 FORMAT ('ERROR',I5,' READING CATALOG FILE')
 1030 FORMAT ('Field',7X,'RA',12X,'Dec',13X,'Flux',5X,'Beam',4X,'Sep')
 1031 FORMAT (I5,4X,2(I2.2,':'),F5.2,3X,A1,2(I2.2,':'),F4.1,F13.3,
     *   F9.3,F8.3)
 1035 FORMAT ('C ',I5,2I6,2X,2I3.2,F6.2,2X,A1,I2.2,I3.2,F5.1)
 1036 FORMAT ('F ',I5,2I6,2X,2F13.4)
 1037 FORMAT ('ERROR NO', I5, ' WRITING BOXFILE IN ADNVSS')
 1040 FORMAT (I5.5,'  -1',3I7)
 1050 FORMAT ('Add NVSS: wrote ',I3,' fields to BOXFILE')
      END
      SUBROUTINE ADDSUN (OBSDAY, RADIUS, CELSIZ, FLDSIZ, MAXANG, RA0,
     *   DEC0, OFIELD, LUNBX, INDBX, WRBOX, OPTYPE, IERR)
C-----------------------------------------------------------------------
C   The Sun moves so should really have an accurate time (or time
C   range).  Should get it from the input file (either NX file or
C   reading UV dataset) and pass it here.
C   Inputs:
C      OBSDAY   I(6)   Date of observation: YYY, MM, DD, HH, MM, SS
C      RADIUS   R      Inner radius in degrees: skip if Sun < RADIUS
C      CELSIZ   R      Cell size in arc sec
C      FLDSIZ   I      Desired FLDSIZE parameter for Sun field
C      MAXANG   R      Max angle from Sun to center to include (deg)
C      RA0      D      Center RA in degrees
C      DEC0     D      center Dec in degrees
C      LUNBX    I      LUN of box file
C      INDBX    I      FTAB pointer for box file
C      WRBOX    I   Write Clean boxes for sources
C   In/Out:
C      OFIELD   I      Number of fields already written
C   Output:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      DOUBLE PRECISION DEC0, RA0
      REAL      RADIUS, CELSIZ(2), MAXANG
      INTEGER   OBSDAY(6), FLDSIZ, OFIELD, LUNBX, INDBX, WRBOX, IERR
      CHARACTER OPTYPE*4
C
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION SUNRA, SUNDEC, SEPN, SINDEC, COSDEC, SINSUN,
     *   COSSUN, SRA, DECSEC, RASEC, XRA, XDEC
      REAL      FLYADD
      INTEGER   RAHR, RAMIN, DECDEG, DECMIN, LFLD, I1, I2, I3, IT, JTRIM
      CHARACTER OUTLIN*48, DECS*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      SINDEC = SIN (DEC0*DG2RAD)
      COSDEC = COS (DEC0*DG2RAD)
      SRA = RA0 * DG2RAD
      CALL SUNPOS (OBSDAY, SUNRA, SUNDEC)
      SINSUN = SIN (SUNDEC)
      COSSUN = COS (SUNDEC)
      SEPN = SINSUN * SINDEC + COSSUN * COSDEC * COS(SUNRA - SRA)
      IF (SEPN.GT.1.0D0) THEN
         SEPN = 0.0D0
      ELSE
         SEPN = RAD2DG * ACOS (SEPN)
         END IF
      FLYADD = FLDSIZ * CELSIZ(1) / 3600.
      WRITE (MSGTXT,1000) SEPN
      WRITE (MSGTXT(12:16), '(F5.1)') SEPN
      IF (SEPN.LE.RADIUS) THEN
         MSGTXT(29:) = 'in the flys eye, so not adding Sun field!'
      ELSE IF (SEPN.LT.MAXANG) THEN
         MSGTXT(29:) = 'adding Sun field'
C                                       add it in...
         SUNRA = SUNRA * RAD2DG / 15.0D0
         SUNDEC = SUNDEC * RAD2DG
         IF (SUNRA.LT.0.0D0) SUNRA = SUNRA + 24.0D0
         IF (SUNDEC.LT.0.0D0) THEN
            DECS = '-'
            SUNDEC = ABS (SUNDEC)
         ELSE
            DECS = ' '
            END IF
         CALL DTRANS (SUNDEC, DECDEG, DECMIN, DECSEC)
         CALL DTRANS (SUNRA, RAHR, RAMIN, RASEC)
         OFIELD = OFIELD + 1
         IF (OPTYPE.NE.'USEF') THEN
            WRITE (OUTLIN,1030) OFIELD, FLDSIZ, FLDSIZ, RAHR, RAMIN,
     *         RASEC, DECS, DECDEG, DECMIN, DECSEC
            IF (OUTLIN(29:29).EQ.' ') OUTLIN(29:29) = '0'
            IF (OUTLIN(43:43).EQ.' ') OUTLIN(43:43) = '0'
         ELSE
            XRA = (SUNRA*15.0D0 - RA0)
            IF (XRA.GT.180.0D0) XRA = XRA - 360.0D0
            IF (XRA.LT.-180.0D0) XRA = XRA + 360.0D0
            XRA = XRA * 3600.0D0 * COS (DEC0*DG2RAD)
            XDEC = (SUNDEC - DEC0) * 3600.0
            WRITE (OUTLIN,1031) OFIELD, I1, I2, XRA, XDEC
            END IF
         IT = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', LUNBX, INDBX, OUTLIN(:IT), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1032) IERR
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       Clean box too
         IF (WRBOX.GT.0) THEN
            CALL POWER2 (FLDSIZ, LFLD)
            I1 = LFLD / 2
            I2 = I1 + 1
            I3 = MIN (LFLD/2-6, WRBOX)
            WRITE (OUTLIN,1040) OFIELD, I3, I1, I2
            IT = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNBX, INDBX, OUTLIN(:IT), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1032) IERR
               CALL MSGWRT (6)
               GO TO 999
               END IF
            END IF
      ELSE
         MSGTXT(29:) = 'ignoring'
         END IF
      CALL MSGWRT (3)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('The Sun is',F6.1,' deg away - ')
 1030 FORMAT ('C ',I5,2I6,2X,2I3.2,F6.2,2X,A1,I2.2,I3.2,F5.1)
 1031 FORMAT ('F ',I5,2I6,2X,2F13.4)
 1032 FORMAT ('ERROR NO', I5, ' WRITING BOXFILE IN ADDSUN')
 1040 FORMAT (I5.5,'  -1',3I7)
      END
      SUBROUTINE SUNPOS (OBSDAY, SUNRA, SUNDEC)
C-----------------------------------------------------------------------
C   Routine to find sun position.  this is crude, and only accurate to
C   some 10's of arcsec, but should be good enough for what we need
C   here.  I used the algorithm from Meeus, chapter 24, pp151-153.
C   Inputs:
C      OBSDAY   I(6)   Date,time: YYYY, MM, DD, HH, MM, SS
C   Outputs:
C      SUNRA    D      Sun RA in radians
C      SUNDEC   D      Sun Dec in radians
C-----------------------------------------------------------------------
      DOUBLE PRECISION SUNRA, SUNDEC
      INTEGER   OBSDAY(6)

      DOUBLE PRECISION JD, T, L0, M, E, C, SUNLON, V, R, OMEGA,
     *   LAMBDA, SL2000, EPS, EPS0
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL DAT2JD (OBSDAY, JD)
C                                       eqn 24.1
      T = (JD - 2451545.0D0) / 36525.0D0
C                                       eqn 24.2
      L0 = 2.8046645D2 + 3.600076983D4 * T + 3.032D-4 * T * T
 10   IF (L0.LT.0.0D0) THEN
         L0 = L0 + 360.0D0
         GO TO 10
         END IF
 20   IF (L0.GT.360.0D0) THEN
         L0 = L0 - 360.0D0
         GO TO 20
         END IF
C                                       eqn 24.3
      M = 3.575291D2 + 3.59990503D4 * T - 1.559D-4 * T * T -
     *    4.8D-7 * T * T * T
 30   IF (M.LT.0.0D0) THEN
         M = M + 360.0D0
         GO TO 30
         END IF
 40   IF (M.GT.360.0D0) THEN
         M = M - 360.0D0
         GO TO 40
         END IF
C                                       eqn 24.4
      E = 1.6708617D-2 - 4.2037D-5 * T - 1.236D-7 * T * T
      M = M * PI / 180.0D0
      C = SIN (M) * (1.9146D0 - 4.871D-3 * T - 1.4D-5 * T * T) +
     *    SIN (2.0D0*M) * (1.9993D-2 - 1.01D-4 * T) +
     *    SIN (3.0D0*M) * 2.9D-4
      SUNLON = L0 + C
      V = M + C * PI / 180.0D0
C                                       eqn 24.5
      R = 1.000001018 * (1 - E * E) / (1 + E * COS (V))
      OMEGA = 1.2504D2 - 1.934136D3 * T
C                                       we want apparent positions,
C                                       though the difference should be
C                                       small...
C     LAMBDA = SUNLON - 5.69D-3 - 4.78D-3 * SIN (OMEGA * PI / 180.0D0)
      SL2000 = SUNLON - 1.397D-2 * (OBSDAY(1) - 2000.0D0)
      LAMBDA = SL2000 - 5.69D-3 - 4.78D-3 * SIN (OMEGA * PI / 180.0D0)
      EPS0 = 23.0D0 + 26.0D0 / 60.0D0 +
     *   (21.448D0 - 46.815D0 * T - 5.9D-4 * T * T +
     *   1.813D-3 * T * T * T) / 3600.0D0
      EPS = EPS0 + 2.56D-3 * COS (OMEGA * DG2RAD)
      EPS = EPS * DG2RAD
      LAMBDA = LAMBDA * DG2RAD
C                                       eqn 24.6
      SUNRA = ATAN2 (COS (EPS) * SIN (LAMBDA), COS (LAMBDA))
      IF (SUNRA.LT.0.0) SUNRA = SUNRA + 2.0D0 * PI
C                                       eqn 24.7
      SUNDEC = DASIN (SIN (EPS) * SIN (LAMBDA))
C
 999  RETURN
      END
      SUBROUTINE DTRANS (ABSVAL, DEGHR, MINUTE, SECOND)
C-----------------------------------------------------------------------
C   translates coordinate into sexagesimal
C   Inputs:
C      ABSVAL   D   Coordinate (with sign)
C   Outputs:
C      DEGHR    I   Degrees (or hours if ABSVAL so scaled) with sign
C      MINUTE   I   Minutes (no sign)
C      SECOND   I   Seconds (no sign)
C-----------------------------------------------------------------------
      DOUBLE PRECISION ABSVAL
      INTEGER   DEGHR, MINUTE
C
      DOUBLE PRECISION SECOND, VALCOP
C-----------------------------------------------------------------------
      VALCOP = ABSVAL
      DEGHR = INT (VALCOP)
      VALCOP = 60.0D0 * DABS (VALCOP - DEGHR)
      MINUTE = INT (VALCOP)
      SECOND = 60.0D0 * (VALCOP - MINUTE)
C
 999  RETURN
      END
      SUBROUTINE BUBBLE (ARRAY1, ARRAY2, NUMBER, DIRECT)
C-----------------------------------------------------------------------
C   In place bubble sort with tracking of swap indices
C   Inputs:
C      NUMBER   I      Number of samples to sort
C      DIRECT   I      Direction of sort (-1 descending)
C   In/Out:
C      ARRAY1   R(*)   Array to sort
C      ARRAY2   I(*)   Array rearranged along with ARRAY1
C-----------------------------------------------------------------------
      REAL      ARRAY1(*)
      INTEGER   ARRAY2(*), NUMBER, DIRECT
C
      INTEGER   I, J, IT
      REAL      RT
      LOGICAL   DONE
C-----------------------------------------------------------------------
      DONE = .FALSE.
      I = 1
C                                       descending order sort
      IF (DIRECT.EQ.-1) THEN
 10      IF ((I.LT.NUMBER) .AND. (.NOT.DONE)) THEN
            DONE = .TRUE.
            DO 20 J = NUMBER,I+1,-1
               IF (ARRAY1(J).GT.ARRAY1(J-1)) THEN
                  RT = ARRAY1(J)
                  ARRAY1(J) = ARRAY1(J-1)
                  ARRAY1(J-1) = RT
                  IT = ARRAY2(J)
                  ARRAY2(J) = ARRAY2(J-1)
                  ARRAY2(J-1) = IT
                  DONE = .FALSE.
                  END IF
 20            CONTINUE
            I = I + 1
            GO TO 10
            END IF
C                                       ascending order sort
      ELSE
 30      IF ((I.LT.NUMBER) .AND. (.NOT.DONE)) THEN
            DONE = .TRUE.
            DO 40 J = NUMBER,I+1,-1
               IF (ARRAY1(J).LT.ARRAY1(J-1)) THEN
                  RT = ARRAY1(J)
                  ARRAY1(J) = ARRAY1(J-1)
                  ARRAY1(J-1) = RT
                  IT = ARRAY2(J)
                  ARRAY2(J) = ARRAY2(J-1)
                  ARRAY2(J-1) = IT
                  DONE = .FALSE.
                  END IF
 40            CONTINUE
            I = I + 1
            GO TO 30
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE SETCEL (DEC0, RADIUS, SCALE, IMSIZE, CELSIZ, IERR)
C-----------------------------------------------------------------------
C   SETCEL reads the data to determine the max baseline and W and
C   determines the appropriate cellsize and imsize
C   Inputs
C      DEC0     D      Declination (deg)
C      RADIUS   R      Maximum radius to use (deg)
C      SCALE    R(2)   Max phase error, points per beam
C   Output:
C      IMSIZE   I(2)   Image size to use
C      CELSIZ   R(2)   Cell size in
C      IERR     I      Error code: > 0 failed
C-----------------------------------------------------------------------
      INTEGER   IMSIZE(2), IERR
      REAL      RADIUS, SCALE(2), CELSIZ(2)
      DOUBLE PRECISION DEC0
C
      INTEGER   J, NW
      REAL      B, W, R, RPARM(20), VIS(24), FS, HPBW, MAXFR, AW, AWW,
     *   WM, PE
      LOGICAL   LIMIT, CEZERO
      DOUBLE PRECISION DARG, DDEC
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IF (SCALE(1).LE.0.0) SCALE(1) = 45.0
      IF (SCALE(2).LE.0.0) SCALE(2) = 3.0
C                                       get elevation
      DARG = 1.0D0
      DDEC = DEC0 * DG2RAD
      DO 10 J = 1,MAXANT
         IF (STNLAT(J).NE.0.0D0) THEN
            DARG = SIN (STNLAT(J)) * SIN (DDEC) + COS (STNLAT(J)) *
     *         COS (DDEC)
            GO TO 20
            END IF
 10      CONTINUE
C                                       minimize data
 20   BCHAN = 1 + 0.4 * CATBLK(KINAX+JLOCF)
      ECHAN = BCHAN
      BIF = 1
      EIF = 1
      CALL UVGET ('INIT', RPARM, VIS, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'INIT', IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      B = 0
      WM = 0.0
      NW = 0
      AW = 0.0
      AWW = 0.0
      CEZERO = (CELSIZ(1).LE.0.0) .OR. (CELSIZ(2).LE.0.0)
C                                       read loop
 100  CALL UVGET ('READ', RPARM, VIS, IERR)
      IF (IERR.NE.-1) THEN
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'READ', IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         R = RPARM(1+ILOCU)**2 + RPARM(1+ILOCV)**2
         B = MAX (B, R)
         W = ABS(RPARM(1+ILOCW))
         WM = MAX (WM, W)
         AW = AW + W
         AWW = AWW + W*W
         NW = NW + 1
         GO TO 100
         END IF
C                                       close file
      CALL UVGET ('CLOS', RPARM, VIS, IERR)
      IF ((W.LE.0.0) .OR. (B.LE.0.0)) THEN
         IERR = 2
         MSGTXT = 'NO DATA FOUND: SET IMSIZE, CELLSIZE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       set results
      B = SQRT (B)
      WRITE (MSGTXT,1005) B, WM
      CALL MSGWRT (4)
      AW = AW / NW
      AWW = SQRT (AWW / NW)
      WRITE (MSGTXT,1008) AW, AWW
      CALL MSGWRT (4)
      AW = MAX (4.0*AW, 3.0*AWW)
      PE = SCALE(1) * DARG
      IF (AW.GT.WM) THEN
         PE = PE * WM / AW
         MSGTXT = 'SETCEL: Correcting for zenith angle, ' //
     *      ' large average W :'
      ELSE
         MSGTXT = 'SETCEL: Correcting only for zenith angle :'
         END IF
      CALL MSGWRT (4)
      WRITE (MSGTXT,1009) PE
      CALL MSGWRT (4)
C                                       min fringe spacing in asec
      FS = RAD2AS / B
C                                       basic cell FS/3
      FS = FS / SCALE(2)
C                                       image size limited by W term
C                                       beam in radian
      HPBW = 1.0 / WM
C                                       max field
      MAXFR = SQRT (HPBW * PE / 180.0)
      R = MAXFR * RAD2AS
      WRITE (MSGTXT,1006) R
      CALL MSGWRT (4)
C                                       limit to RADIUS ?
      IF (RADIUS.GT.0.0) THEN
         R = RADIUS * DG2RAD
         LIMIT = R.LT.MAXFR
         IF (LIMIT) THEN
            WRITE (MSGTXT,1007) (RADIUS*3600.0)
            CALL MSGWRT (4)
            MAXFR = R
            IF ((CEZERO) .AND. ((IMSIZE(1).LE.0) .OR. (IMSIZE(2).LE.0)))
     *         THEN
               CELSIZ(1) = FS
               CELSIZ(2) = FS
               END IF
            END IF
         END IF
C                                       circular Clean
      J = 2 * MAXFR * RAD2AS / FS + 0.5
C                                       message
      WRITE (MSGTXT,1010) J, FS
      CALL MSGWRT (4)
C                                       now set IMSIZE given CELL
      IF ((CELSIZ(1).GT.0.0) .AND. (CELSIZ(2).GT.0.0)) THEN
         J = 2 * MAXFR * RAD2AS / CELSIZ(1) + 10.5
         IF (J.GT.8600) THEN
            IMSIZE(1) = 16384
         ELSE IF (J.GT.4300) THEN
            IMSIZE(1) = 8192
         ELSE IF (J.GT.2150) THEN
            IMSIZE(1) = 4096
         ELSE IF (J.GT.1075) THEN
            IMSIZE(1) = 2048
         ELSE IF (J.GT.540) THEN
            IMSIZE(1) = 1024
         ELSE IF (J.GT.272) THEN
            IMSIZE(1) = 512
         ELSE IF (J.GT.140) THEN
            IMSIZE(1) = 256
         ELSE IF (J.GT.70) THEN
            IMSIZE(1) = 128
         ELSE
            IMSIZE(1) = 64
            END IF
         IMSIZE(2) = IMSIZE(1)
C                                       compromise if init was 0
         IF (CEZERO) THEN
            CELSIZ(1) = CELSIZ(1)/2 + MAXFR * RAD2AS / (IMSIZE(1) - 20.)
            CELSIZ(2) = CELSIZ(1)
            END IF
C                                       or set CELL given IMSI
      ELSE IF ((IMSIZE(1).GT.0) .AND. (IMSIZE(2).GT.0)) THEN
         CELSIZ(1) = 2. * MAXFR * RAD2AS / (IMSIZE(1) - 10.)
         CELSIZ(2) = CELSIZ(1)
C                                       balance the two
      ELSE
         J = 2 * MAXFR * RAD2AS / FS + 20.5
         IF (J.GT.8600) THEN
            IMSIZE(1) = 16384
         ELSE IF (J.GT.4300) THEN
            IMSIZE(1) = 8192
         ELSE IF (J.GT.2150) THEN
            IMSIZE(1) = 4096
         ELSE IF (J.GT.1075) THEN
            IMSIZE(1) = 2048
         ELSE IF (J.GT.540) THEN
            IMSIZE(1) = 1024
         ELSE IF (J.GT.272) THEN
            IMSIZE(1) = 512
         ELSE IF (J.GT.140) THEN
            IMSIZE(1) = 256
         ELSE IF (J.GT.70) THEN
            IMSIZE(1) = 128
         ELSE
            IMSIZE(1) = 64
            END IF
         IMSIZE(2) = IMSIZE(1)
         CELSIZ(1) = 2. * MAXFR * RAD2AS / (IMSIZE(1) - 20.)
         CELSIZ(2) = CELSIZ(1)
         END IF
C                                       message
      WRITE (MSGTXT,1020) IMSIZE(1), CELSIZ(1)
      CALL MSGWRT (4)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UV DATA ',A,' ERROR',I5)
 1005 FORMAT ('SETCEL: max baseline, W =',2(1PE11.4),' wavelengths')
 1006 FORMAT ('SETCEL: max field radius',1PE11.4,' arc seconds')
 1007 FORMAT ('SETCEL: is > requested radius',1PE11.4,' arc seconds')
 1008 FORMAT ('SETCEL: average, rms  W =',2(1PE11.4),' wavelengths')
 1009 FORMAT ('SETCEL: to use phase error',F6.1,' deg at max W')
 1010 FORMAT ('SETCEL: recommends IMSIZE',I6,' CELLSIZE',F11.6)
 1020 FORMAT ('SETCEL: returns    IMSIZE',I6,' CELLSIZE',F11.6)
      END
      SUBROUTINE SETIME (DISK, CATNO, SOUID, START, STOP, IERR)
C-----------------------------------------------------------------------
C   Determines the start and stop times of a uv data set.  For single-
C   source, it reads the first and last points which is only
C   representative if the data are not 'T?' sorted.  For multi-source,
C   it reads the NX index file.
C   Input:
C      DISK     I   Disk number
C      CATNO    I   Catalog number
C      SOUID    I   Source number (-1 all)
C   Output:
C      START    R   Start time (days)
C      STOP     R   Stop time in days
C      IERR     I   Return code, 0=>OK, otherwise INDEX file
C                        exists but cannot be read.
C      Requires - UVPGET to have been called
C               - SOUFIL to have been called (multi-source)
C               - uv header in common /MAPHDR/ (I think)
C-----------------------------------------------------------------------
      REAL      START, STOP
      INTEGER   DISK, CATNO, SOUID, IERR
C
      INTEGER   JERR, NXIDSO, NXSUBA, NIO, IUBIND, I, NXSTA, NXEND,
     *   NREAD, BO, FRSRED, FREQID
      LOGICAL   T, F, TABLE, EXIST, FITASC
      REAL      NXTIME, NXDTIM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BO /1/
C-----------------------------------------------------------------------
C                                       Initialize output
      INXRNO = -1
      FSTVIS = 1
      LSTVIS = CATBLK(KIGCN)
      CURSOU = 0
      IXLUN = 30
      NINDEX = 0
      START = 1.E6
      STOP = -START
C                                       See if NX file exists.
      IF (SOUID.GT.0) THEN
         CALL ISTAB ('NX', DISK, CATNO, 1, IXLUN, NXBUFF, TABLE, EXIST,
     *      FITASC, JERR)
C                                       Open NX table
         IF ((JERR.EQ.0) .AND. (TABLE) .AND. (EXIST)) THEN
            CALL NDXINI ('READ', NXBUFF, DISK, CATNO, 1, CATBLK, IXLUN,
     *         INXRNO, NXKOLS, NXNUMV, JERR)
            IF (JERR.GT.0) THEN
               IERR = 1
               WRITE (MSGTXT,1000) 'OPEN', JERR
               GO TO 990
               END IF
C                                       Get number of scans
            NINDEX = NXBUFF(5)
            END IF
         END IF
C                                       Locate first selected scan.
C                                       Loop through records
      IF (NINDEX.GT.0) THEN
         DO 20 I = 1,NINDEX
            INXRNO = I
C                                       Read record.
            CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV, NXTIME,
     *         NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) 'READ', IERR
               GO TO 990
            ELSE IF (IERR.EQ.0) THEN
               IF (NXIDSO.EQ.SOUID) THEN
                  START = MIN (START, NXTIME - NXDTIM/2.0)
                  STOP = MAX (STOP, NXTIME + NXDTIM/2.0)
                  END IF
               END IF
 20         CONTINUE
         CALL TABIO ('CLOS', 0, INXRNO, UBUFF, NXBUFF, IERR)
C                                       No INDEX file
      ELSE
         INXRNO = -1
         FSTVIS = 1
         LSTVIS = CATBLK(KIGCN)
         NINDEX = 0
         NXBUFF(5) = NINDEX
C                                       open uv file
         IULUN = 25
C                                       Open UV file
         CALL ZPHFIL ('UV', DISK, CATNO, 1, UFILE, IERR)
         CALL ZOPEN (IULUN, IUFIND, DISK, UFILE, T, F, T, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1800) 'OPEN', IERR
            GO TO 990
            END IF
C                                       Init I/O to uvfile
         NREAD = 1
         LRECIN = LREC
         UBUFSZ = UVBFSL * 2
C                                       read first vis
         FRSRED = FSTVIS - 1
         CALL UVINIT ('READ', IULUN, IUFIND, NREAD, FRSRED, LRECIN, 1,
     *      UBUFSZ, UBUFF, BO, IUBIND, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1810) 'INIT FIRST', IERR
            GO TO 980
            END IF
         CALL UVDISK ('READ', IULUN, IUFIND, UBUFF, NIO, IUBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1810) 'READ FIRST', IERR
            GO TO 980
            END IF
         START = UBUFF(IUBIND+ILOCT)
C                                       read last vis
         FRSRED = LSTVIS - 1
         CALL UVINIT ('READ', IULUN, IUFIND, NREAD, FRSRED, LRECIN, 1,
     *      UBUFSZ, UBUFF, BO, IUBIND, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1810) 'INIT LAST', IERR
            GO TO 980
            END IF
         CALL UVDISK ('READ', IULUN, IUFIND, UBUFF, NIO, IUBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1810) 'READ LAST', IERR
            GO TO 980
            END IF
         STOP = UBUFF(IUBIND+ILOCT)
         CALL ZCLOSE (IULUN, IUFIND, JERR)
         END IF
      GO TO 999
C                                       Errors
 980  CALL MSGWRT (8)
      CALL ZCLOSE (IULUN, IUFIND, JERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SETIME: ERROR',I3,' ',A,'ING INDEX TABLE')
 1800 FORMAT ('SETIME: UNABLE TO ',A,' INPUT UV FILE, ERROR',I5)
 1810 FORMAT ('SETIME: UNABLE TO ',A,' INPUT VISIBILITY RECORD, ERROR',
     *   I5)
      END
       SUBROUTINE POWER2 (WORDS, P2SIZE)
C-----------------------------------------------------------------------
C   POWER2 determines the largest power of 2 JUST exceeding or = WORDS.
C   Input:
C      WORDS    I   Size just to exceed
C   Output:
C      P2SIZE   I   Largest power of 2 just exceeding WORDS
C-----------------------------------------------------------------------
       INTEGER   WORDS, P2SIZE, I, SIZE
C-----------------------------------------------------------------------
      SIZE = 1
      DO 10 I = 1,22
         IF (SIZE.LT.WORDS) SIZE = SIZE * 2
 10      CONTINUE
C
      P2SIZE = SIZE
C
 999  RETURN
      END
