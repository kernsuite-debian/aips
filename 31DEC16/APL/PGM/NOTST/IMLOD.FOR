LOCAL INCLUDE 'IMLOD.INC'
C                                       Local include for IMLOD
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INTEGER   IBLNK
      INTEGER   FDVEC(50), TBIND, NTAPE, CNO, NBPIX, TAPEBP, TABLES,
     *   INBUFF(MABFSS), TAPBUF(29184), UNKNWN, USED(300), KEEP
      LOGICAL   ISBLNK, FUCKUP, STDEXT, DODISK
      REAL      PCMATX(7,7), CDMATX(7,7), PVMATX(7,7)
      HOLLERITH XNAME(3), XCLASS(2), XNAME0(3), XCLAS0(2), XINFIL(12),
     *   XJNFIL(12)
      CHARACTER TNAME*48, NAME*12, CLASS*6, INFILE*48, HDRBUF*2880
      DOUBLE PRECISION POS11(2), SCALE, OFFSET, ISCALE, IZERO
      REAL      NTAPE4, SEQ4, KVOL4, NCOUNS, DOTABL, NFILES, NMAPS,
     *   XERR4
      REAL      NTAPE0, SEQ0, KVOL0, NCOUN0, DOTAB0, NFILE0, NMAPS0,
     *   XERR0
      COMMON /MLTAP/ SCALE, OFFSET, ISCALE, IZERO, POS11, TAPBUF,
     *   INBUFF, IBLNK, ISBLNK, FUCKUP, STDEXT, DODISK, UNKNWN,
     *   FDVEC, TBIND, NTAPE, CNO, NBPIX, TAPEBP, TABLES, PCMATX,
     *   CDMATX, PVMATX, USED, KEEP
      COMMON /INPARM/ NTAPE4, XNAME, XCLASS, SEQ4, KVOL4, NCOUNS,
     *   DOTABL, NFILES, NMAPS, XINFIL, XERR4
      COMMON /DUMPAR/ NTAPE0, XNAME0, XCLAS0, SEQ0, KVOL0, NCOUN0,
     *   DOTAB0, NFILE0, NMAPS0, XJNFIL, XERR0
      COMMON /CHRCOM/ HDRBUF, NAME, CLASS, INFILE, TNAME
C                                                          End IMLOD
LOCAL END
      PROGRAM IMLOD
C-----------------------------------------------------------------------
C! Reads images from tape or FITS disk file
C# Tape Map-util EXT-util FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2003-2004, 2007-2012, 2015
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
C   IMLOD is the main control program for conversion of the IBM-360
C   DEC FORMAT map or a FITS FORMAT map into the standard map format.
C   AIPS input parameters:
C      INTAPE        R     Input tape unit #
C      OUTNAME(3)    R     Image name (name)
C      OUTCLASS(2)   R     Image name (class)
C      OUTSEQ        R     Image name (seq #) (< 0 => tape value)
C      OUTDISK       R     Output disk unit #
C      NCOUNT        R     Number of images to load.
C      DOTABLE       R     False means skip any tables. (FITS only).
C      NFILES        R     Number of files to skip first (<0 back, 0 to
C                          current BOF, > 0 forward)
C      NMAPS         R     Number IBM maps to skip forward after file
C                          positioning
C-----------------------------------------------------------------------
      INTEGER   IRET, SEQ, KVOL, HLUN, HBUFF(256), FITS, ISCR(256),
     *   I, N, IERR, NPARM, NMPSKP, IROUND, LFITS, JDEST
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA HLUN, NPARM /27, 25/
C-----------------------------------------------------------------------
      LFITS = 0
      CALL MLINI (NPARM, KVOL, SEQ, ISCR, IRET)
      IF (IRET.NE.0) GO TO 995
      UNKNWN = 0
C                                       Convert to internal format.
      N = NCOUNS + .05
      IF (N.LE.0) N = 1
      IF (DODISK) N = 1
C                                       Open and position tape.
      NMPSKP = IROUND (NFILES)
      CALL MLTAPE (NMPSKP, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       IBM format positioning
      NMPSKP = IROUND (NMAPS)
      CALL RCOPY (NPARM, NTAPE4, NTAPE0)
C                                       Loop over requested images
      DO 100 I = 1,N
C                                       Virgin copy of parms
 20      CALL RCOPY (NPARM, NTAPE0, NTAPE4)
         CALL FILL (300, 0, USED)
         KEEP = 0
C                                       See what kind of file.
         IF (IRET.NE.0) LFITS = 0
         CALL TPIOHD (FDVEC, 128, FITS, TBIND, TAPBUF, ISCR, IRET)
         IF ((LFITS.EQ.2) .AND. (IRET.EQ.4)) GO TO 20
         IF (IRET.NE.0) GO TO 950
         IF ((FITS.NE.1) .AND. (FITS.NE.2)) THEN
            WRITE (MSGTXT,1000)
            IF (FITS.EQ.-1) WRITE (MSGTXT,1001)
            IF ((FITS.EQ.-1) .AND. (FDVEC(42).EQ.2560))
     *         WRITE (MSGTXT,1002)
            CALL MSGWRT (8)
            IRET = 1
            GO TO 950
            END IF
C                                       The tape is recognizable
         LFITS = FITS
         IF (FITS.EQ.1) CALL FITTAP (HLUN, HBUFF, KVOL, IRET)
         IF (FITS.EQ.2) CALL DECTAP (NMPSKP, HLUN, HBUFF, ISCR, IRET)
         IF (IRET.NE.0) GO TO 950
         IF (KEEP.GE.0) THEN
            CALL IMQUIT
         ELSE
            MSGTXT = 'Destroying dummy image'
            CALL MSGWRT (3)
            CALL MDESTR (KVOL, CNO, CATBLK, HBUFF, JDEST, IERR)
            END IF
 100     CONTINUE
C                                       Close output
 950  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
      IF (IRET.EQ.0) IRET = IERR
C                                       Clean up
 995  CALL DIE (IRET, HBUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('TAPE IS UV-EXPORT FORMAT: USE UVLOD')
 1001 FORMAT ('TAPE FILE IS NON-STANDARD FITS WHICH IS NOT SUPPORTED')
 1002 FORMAT ('TAPE IS LIKELY TO BE ''RPFITS'' FORMAT: TRY ATLOD')
      END
      SUBROUTINE MLINI (NPARM, KVOL, SEQ, ISCR, IRET)
C-----------------------------------------------------------------------
C   MLINI initializes IMLOD
C   Outputs:
C      KVOL   I     Output disk #
C      SEQ    I     Output sequence number
C      ISCR   I(256)   Scratch buffer.
C      IRET   I     Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   KVOL, ISCR(256), IRET, SEQ, I1
      INTEGER   NPARM, IERR, IROUND
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'IMLOD '/
C-----------------------------------------------------------------------
C                                       Initialize disk character.
      I1 = 1
      CALL ZDCHIN (.TRUE.)
      CALL HIINIT (3)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       Initialize for AIPS
      IRET = 0
      CALL GTPARM (PRGNAM, NPARM, RQUICK, NTAPE4, ISCR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         RQUICK = .FALSE.
         GO TO 990
         END IF
C                                       Check if disk file output
      CALL H2CHR (48, 1, XINFIL, INFILE)
      DODISK = INFILE.NE.' '
      IF (((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) .AND.
     *   (.NOT.DODISK)) THEN
         WRITE (MSGTXT,1020)
         RQUICK = .FALSE.
         GO TO 990
         END IF
      IF (RQUICK) CALL RELPOP (IRET, ISCR, IERR)
C                                       Fill in parameters and defaults
      SEQ = IROUND (SEQ4)
      NTAPE = IROUND (NTAPE4)
      IF (NTAPE.EQ.0) NTAPE = 1
      KVOL = IROUND (KVOL4)
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
C                                       Init FDVEC
      CALL FILL (50, 0, FDVEC)
      GO TO 999
C
 990  IRET = 16
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR IN GTPARM.  IER=',I7)
 1020 FORMAT ('TAPES USED ONLY WITH INTERACTIVE AIPS')
      END
      SUBROUTINE DECTAP (NMPSKP, HLUN, HBUFF, ISCR, IERR)
C-----------------------------------------------------------------------
C   Process IBM DEC type tape header and data.
C   Inputs:
C      HLUN   I        Open History file LUN.
C   In/outs:
C      NMPSKP I        Number maps to advance before accepting the first
C      HBUFF  I(256)   History file work I/O buffer.
C      ISCR   I(256)   Scratch I/O buffer.
C   Outputs:
C      IERR   I        Error code, 0=ok.
C-----------------------------------------------------------------------
      INTEGER   NMPSKP, HLUN, HBUFF(256), ISCR(256), IERR
      INTEGER   SEQ, IBL, KVOL, FITS, LIMIT, J
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      KVOL = KVOL4 + .5
      SEQ = SEQ4 + .5
 10   CALL DECHDR (ISCR, IBL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
C                                       Skip images
         IF (NMPSKP.GT.0) THEN
            LIMIT = CATBLK(KINAX+1)
            FDVEC(2) = CATBLK(KINAX) * 2
            FDVEC(31) = 0
            FDVEC(32) = LIMIT
            DO 20 J = 1,LIMIT
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
 20            CONTINUE
            NMPSKP = NMPSKP - 1
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR
               GO TO 990
               END IF
            CALL TPIOHD (FDVEC, 128, FITS, TBIND, TAPBUF, ISCR, IERR)
            IF ((IERR.EQ.0) .AND. (FITS.EQ.2)) THEN
               GO TO 10
            ELSE
               WRITE (MSGTXT,1025) IERR
               IF ((FITS.NE.2) .AND. ((IERR.EQ.0) .OR. (IERR.EQ.10)))
     *            WRITE (MSGTXT,1026)
               GO TO 990
               END IF
            END IF
C                                       Fill in some header defaults
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL MLDEF (NAME, CLASS, SEQ)
      CATBLK(KIIMU) = NLUSER
C                                       force floating
      NBPIX = TAPEBP
C                                       Create slot in CAT file and
C                                       create the map file
      CALL MCREAT (KVOL, CNO, HBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
      NCFILE = 1
      FCNO(1) = CNO
      FRW(1) = 2
      FVOL(1) = KVOL
C                                       Create HI file
      CALL HICREA (HLUN, KVOL, CNO, CATBLK, HBUFF, IERR)
      IF (IERR.NE.0) THEN
           WRITE (MSGTXT,1040) IERR
           GO TO 990
           END IF
C                                       Write HI file
      CALL DECHIS (HLUN, HBUFF, ISCR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                       Add inputs to history file.
      CALL HISINP (KVOL, CNO, HLUN, HBUFF)
      CALL DECDAT (KVOL, IBL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
      GO TO 999
C                                       Error print.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR IN CONVERSION OF HEADER.  IER=',I7)
 1020 FORMAT ('ERROR IN SKIPPING MAPS: IERR =',I5)
 1025 FORMAT ('ERROR IN READING NEXT IMAGE HEADER: IERR =',I5)
 1026 FORMAT ('ERROR IN READING NEXT IMAGE HEADER: NOT IBM FORMAT')
 1030 FORMAT ('COULD NOT CREATE SLOT FOR MAP.  IER=',I7)
 1040 FORMAT ('COULD NOT CREATE HI FILE.  IER=',I7)
 1050 FORMAT ('COULD NOT WRITE HI FILE.  IER=',I7)
 1060 FORMAT ('ERROR IN CONVERSION OF DATA.  IER=',I7)
      END
      SUBROUTINE DECDAT (KVOL, IBL, IER)
C-----------------------------------------------------------------------
C   DECDAT read the map array from an IBM-360 DEC format tape and
C   stores the map in a standard map file.  The header is also
C   cataloged.
C   Inputs:
C      KVOL      I     Disk unit number
C      IBL       I     Number bits used for blanking in IBM format
C      CATBLK(256)  I     Map header
C   Outputs:
C      IER       I     Error return: 0-->  okay
C                                    1-->  problem
C-----------------------------------------------------------------------
      INTEGER   KVOL, IER, IBL
C
      CHARACTER MNAME*48
      INTEGER   IERR, DLUN, MIND, NX, NY, WINDOW(4), NBKOF, OUTIND, I,
     *   NBY, IX
      LOGICAL   T, F
      INCLUDE 'IMLOD.INC'
      REAL      BUFOUT(MABFSS)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      DLUN = 16
      IER = 0
C                                       Get mapfile name
      CALL ZPHFIL ('MA', KVOL, CNO, 1, MNAME, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 970
         END IF
C                                       Open mapfile
      CALL ZOPEN (DLUN, MIND, KVOL, MNAME, T, F, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 970
         END IF
C                                       Find and check buffer size
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
C                                       Initialize disk.  Map is
C                                       upside down
      WINDOW(1) = 1
      WINDOW(2) = NY
      WINDOW(3) = NX
      WINDOW(4) = 1
      NBY = MABFSS * 2
      NBKOF = 1
      CALL MINIT ('WRIT', DLUN, MIND, NX, NY, WINDOW, BUFOUT, NBY,
     *   NBKOF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 970
         END IF
C                                       Init tape I/O
      FDVEC(2) = NX * 2
      FDVEC(31) = 0
      FDVEC(32) = NY
C                                       Double buffer read/write
      DO 100 I = 1,NY
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1070) IERR,I
            GO TO 970
            END IF
         CALL MDISK ('WRIT', DLUN, MIND, BUFOUT, OUTIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1080) IERR,I
            GO TO 970
            END IF
C                                       Change byte order, if nec.
         CALL ZI16IL (NX, 1, TAPBUF(TBIND), INBUFF)
         CALL SNEVAL (NX, IBL, 1, BLANKV, INBUFF)
         DO 95 IX = 1,NX
            IF (INBUFF(IX).EQ.BLANKV) THEN
               BUFOUT(OUTIND+IX-1) = FBLANK
               CATR(KRBLK) = FBLANK
            ELSE
               BUFOUT(OUTIND+IX-1) = INBUFF(IX) * SCALE + OFFSET
               END IF
 95         CONTINUE
 100     CONTINUE
C                                       Write final buffer
      CALL MDISK ('FINI', DLUN, MIND, BUFOUT, OUTIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 970
         END IF
C                                       Normal close-out
      CALL MAPCLS ('WRIT', KVOL, CNO, DLUN, MIND, CATBLK, T, BUFOUT,
     *   IERR)
      NCFILE = NCFILE - 1
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110) IERR
         CALL MSGWRT (6)
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                                       Error returns
 970  CALL MSGWRT (8)
      IER = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('COULD NOT FIND PHYSICAL FILE NAME.  IER=',I7)
 1030 FORMAT ('COULD NOT OPEN MAP FILE.  IER=',I7)
 1050 FORMAT ('COULD NOT INITIALIZE MAP FILE.  IER=',I7)
 1070 FORMAT ('ERROR IN READING TAPE.  IER=',I7,' LINE#=',I5)
 1080 FORMAT ('ERROR IN WRITING ON DISK.  IER=',I7,' LINE#=',I5)
 1100 FORMAT ('ERROR IN WRITING LAST LINE ON DISK.  IER=',I7)
 1110 FORMAT ('ERROR IN CLOSING MAP AND CATALOG FILE.  IER=',I7)
      END
      SUBROUTINE SNEVAL (NPIX, SNDIV, SNCUT, SUBVAL, INB)
C----------------------------------------------------------------------
C   SNEVAL corrects a buffer for the presence of blanking substituting
C   a specified value for the blanked pixels.  Unblanked drops through
C   quickly as well.
C   Inputs : NPIX    I*2        Number of pixels in buffers
C            SNDIV   I*2        Header blanking value S2H(K2INH)
C            SNCUT   I*2        S/N >= SNCUT acceptable: n-bit blanking
C            SUBVAL  I*2        Value to give blanked pixels
C   In/out : INB     I*2(NPIX)  Input buffer & output corrected buffer
C----------------------------------------------------------------------
      INTEGER   NPIX, SNDIV, SNCUT, SUBVAL, INB(*)
      INTEGER   I, ITEMP, ISN, DIV, SNC
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
      IF (NPIX.LE.0) GO TO 999
      IF (SNDIV.EQ.0) GO TO 999
      IF (SNDIV.NE.BLANKV) THEN
         IF ((SNDIV.LE.0) .OR. (SNDIV.GT.8)) THEN
            WRITE (MSGTXT,1000) SNDIV
            CALL MSGWRT (6)
            END IF
C                                       Blank with multiple bits

         DIV = 2 ** SNDIV
         SNC = MIN0 (SNCUT, DIV-1)
         DO 20 I = 1,NPIX
            ITEMP = INB(I)
            ISN = MOD (IABS(ITEMP), DIV)
            IF (ISN.GE.SNC) INB(I) = ITEMP/DIV
            IF (ISN.LT.SNC) INB(I) = SUBVAL
 20         CONTINUE
C                                       Blank with magic value
      ELSE IF (SUBVAL.NE.BLANKV) THEN
         DO 120 I = 1,NPIX
            IF (INB(I).EQ.BLANKV) INB(I) = SUBVAL
 120        CONTINUE
         END IF
C
 999  RETURN
C---------------------------------------------------------------------
 1000 FORMAT (' ILLEGAL BLANKING VALUE =',I8)
      END
      SUBROUTINE DECHDR (D2H, IBL, IER)
C-----------------------------------------------------------------------
C   DECHDR converts the header record from the dec-format maps on the
C   IBM into the catalog record header for the standard map data
C   base.
C   Inputs:
C      D2H   I(128)   the array with the dec-format header as read
C   Outputs:
C      D2H   I(128)   the header converted to local char/integer
C      IBL   I        the # bits used for blanking
C      IER   I        error return 0 = Okay   2 = Abort
C   The output header is in the common /MLTAPE/
C-----------------------------------------------------------------------
      INTEGER   D2H(128), IBL, IER
C
      CHARACTER DATE*8, R4IB8*4, AXT(4)*8, BUNIT*8, VLA*8, OBJECT*8
      INTEGER   DAY, MONTH, YEAR, MSKCLS, ND, I, NDIM, ZAND
      REAL      CSPEED
      DOUBLE PRECISION C1, C2, C3, VALUE, TWOPI
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA AXT/'RA---SIN','DEC--SIN','FREQ    ','STOKES  '/
      DATA BUNIT, VLA, R4IB8 /'JY/BEAM ', 'VLA     ', '4IB8'/
      DATA MSKCLS /255/
C-----------------------------------------------------------------------
C                                       Initialization of constants
      TWOPI = 6.28318530717959D0
      C1 = 360.D0 / 65536.D0 / 32768.D0
      C2 = 1.0D0 / 3.6D8
      C3 = 32768.D0
      CSPEED = 299792.9D0
      IER = 0
C                                       Flip bytes around
      CALL ZI16IL (124, 5, D2H(1), D2H(5))
      CALL ZC8CL (8, 1, D2H, OBJECT)
C                                       Clear CATBLK array
      CALL CATINI (CATBLK)
C                                       A few consistency checks.
      IF ((D2H(17).LT.16) .OR. (D2H(17).GT.2048)) THEN
         WRITE (MSGTXT,1010) D2H(17)
         GO TO 990
      ELSE IF ((D2H(18).LT.16) .OR. (D2H(18).GT.2048)) THEN
         WRITE (MSGTXT,1020) D2H(18)
         GO TO 990
         END IF
C                                       Object name or source
      CALL CHR2H (8, OBJECT, 1, CATH(KHOBJ))
C                                       Telescope
      CALL CHR2H (8, VLA, 1, CATH(KHTEL))
C                                       Instrument
      CALL CHR2H (8, VLA, 1, CATH(KHINS))
C                                       Date of observation
      YEAR = D2H(46) - 1900
      DAY = MOD (D2H(45), 256)
      MONTH = (D2H(45) - DAY) / 256
      WRITE (DATE,1030) DAY, MONTH, YEAR
      CALL FILZCH (8, 1, DATE)
      CALL CHR2H (8, DATE, 1, CATH(KHDOB))
C                                       Date of map
      YEAR = D2H(42) - 1900
      DAY = MOD (D2H(41), 256)
      MONTH = (D2H(41) - DAY) / 256
      WRITE (DATE,1030) DAY, MONTH, YEAR
      CALL FILZCH (8, 1, DATE)
      CALL CHR2H (8, DATE, 1, CATH(KHDMP))
C                                       Scale factor of maps
      SCALE = 2.0**D2H(14)
C                                       Zero level of maps
      OFFSET = 0.
C                                       Brightness units
      CALL CHR2H (8, BUNIT, 1, CATH(KHBUN))
C                                       Set number of axes to 4
      NDIM = 4
      CATBLK(KIDIM) = NDIM
C                                       Set number of random parameters
C                                       to zero
      CATBLK(KIPCN) = 0
C                                       Insert axis types
      DO 40 I = 1,NDIM
         CALL CHR2H (8, AXT(I), 1, CATH(KHCTP+2*(I-1)))
 40      CONTINUE
C                                       Number of pixels in each axis
      CATBLK(KINAX) = D2H(17)
      CATBLK(KINAX+1) = D2H(18)
      CALL FILL (5, 1, CATBLK(KINAX+2))
C                                       Axis value at reference pixel
C                                       ra
      CALL ZR8P4 (R4IB8, D2H(27), VALUE)
      CATD(KDCRV) = VALUE * C1
C                                       dec
      CALL ZR8P4 (R4IB8, D2H(29), VALUE)
      CATD(KDCRV+1) = VALUE * C1
C                                       frequency
      CALL ZR8P4 (R4IB8, D2H(9), VALUE)
      CATD(KDCRV+2) = VALUE * 1000.D0
C                                       stokes
C                                       0=B,1=I,2=Q,3=U,4=V
      ND = ZAND (D2H(7), MSKCLS) -1
      IF (ND.GT.13) ND = 1
      I = ND
      IF (I.EQ.7) ND = 8
      IF (I.EQ.8) ND = 7
      IF (ND.LT.0) ND = 1
      CATD(KDCRV+3) = ND
C                                       Axis increments
      CALL ZR8P4 (R4IB8, D2H(19), VALUE)
C                                       ra
      CATR(KRCIC) = -(VALUE * C2)
C                                       dec
      CALL ZR8P4 (R4IB8, D2H(21), VALUE)
      CATR(KRCIC+1) = VALUE * C2
C                                       frequency
      CATR(KRCIC+2) = 0.
C                                       stokes
      CATR(KRCIC+3) = 0.
C                                       Reference pixel
C                                       ra
      CALL ZR8P4 (R4IB8, D2H(31), VALUE)
      CATR(KRCRP) = VALUE * 0.01
C                                       dec
      CALL ZR8P4 (R4IB8, D2H(33), VALUE)
      CATR(KRCRP+1) = D2H(18) + 1. - VALUE * 0.01
C                                       frequency
      CATR(KRCRP+2) = 1.0
C                                       stokes
      CATR(KRCRP+3) = 1.0
C                                       Rotation of axes
C                                       slippage in ra
      CATR(KRCRT) = 0.
C                                       dec into ra
      CATR(KRCRT+1) = D2H(23) / 10.0
C                                       Epoch
      CATR(KREPO) = 1950.0
C                                       Data max and min values
      CATR(KRDMX) = D2H(16) * SCALE + OFFSET
      CATR(KRDMN) = D2H(15) * SCALE + OFFSET
C                                       Undefined pixel value
      IBL = D2H(67)
C                                       Number of bits per pixel
      TAPEBP = 16
C                                       Observing RA and DEC
      CALL ZR8P4 (R4IB8, D2H(47), VALUE)
      CATD(KDORA) = VALUE * C1
      CALL ZR8P4 (R4IB8, D2H(49), VALUE)
      CATD(KDODE) = VALUE * C1
      IF ((CATD(KDODE).EQ.0.0D0) .AND. (CATD(KDORA).EQ.0.0D0)) THEN
         CATD(KDORA) = CATD(KDCRV)
         CATD(KDODE) = CATD(KDCRV+1)
         END IF
C                                       Image name,class and seq
C                                       Type of map
C                                       1 = normal
C                                       2 = components
C                                       3 = residual
C                                       4 = points
      CATBLK(KITYP) = D2H(8)/256
C                                       Number of iterations
      CATBLK(KINIT) = D2H(40)
C                                       Major axis of beam
      IF ((CATBLK(KINIT).GT.0) .AND. (CATR(KRCIC).NE.0.0)) THEN
         CALL ZR8P4 (R4IB8, D2H(35), VALUE)
         CATR(KRBMJ) = C2 * VALUE
C                                       Minor axis of beam
         CALL ZR8P4 (R4IB8, D2H(37), VALUE)
         CATR(KRBMN) = VALUE * C2
C                                       Beam position angle
         CATR(KRBPA) = D2H(39) / 100.0
         END IF
      GO TO 999
C                                       Error return
 990  CALL MSGWRT (8)
      IER = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('X-GRID SIZE IS OUT OF RANGE',I6)
 1020 FORMAT ('Y-GRID SIZE IS OUT OF RANGE',I6)
 1030 FORMAT (I2,'/',I2,'/',I2)
      END
      SUBROUTINE DECHIS (HLUN, HBUFF, D2H, IER)
C-----------------------------------------------------------------------
C   DECHIS writes the HI file associated with a DEC map generated from
C   IMLOD.  This portion of the file contains parameters which went
C   into the observations, mapping and cleaning of the map.  The most
C   important parameters are in the standard header.
C   INPUTS:
C      HLUN      I    Logical unit number
C      HBUFF(256) I   HI work buffer
C      D2H(128)  I    DEC header already converted to local form
C   OUTPUTS:
C      IER       I    Error return
C                        0 -> okay
C                        1 -> error
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, R4IB8*4, BAND(7)*2, TAPTYP(3)*8,
     *   MAPTYP(2,3)*8, CLNPR(5)*8,  CONTYP(6)*8, COMM*40, LINE*80
      INTEGER   HLUN, HBUFF(256), D2H(128), IER, ITEMP1, ITEMP2, MSKLO,
     *   BIT(16), I, IERR, ZAND
      LOGICAL   FLAG, T
      REAL      TEMP1
      DOUBLE PRECISION C1, VALUE, VALUE1, C2
      INCLUDE 'INCS:DMSG.INC'
      DATA T /.TRUE./
      DATA R4IB8 /'4IB8'/
C     DATA MSKLO /Z00FF/
      DATA MSKLO /  255/
      DATA BAND /'L ','C ','U ','K ','X ','S ','? '/
      DATA TAPTYP /'NONE    ','GAUSSIAN','OTHER   '/
      DATA MAPTYP /'DIRTY   ','CLEAN   ','UVGRID  ','MEM     ',
     *   'DATA    ','MODEL   '/
      DATA CLNPR  /'ALL     ','NORMAL  ','COMPON  ','POINTS  ',
     *   'RESIDUAL'/
      DATA CONTYP /'PILL-BOX','GAUSSIAN','SINC    ','KAI-BESS',
     *   'HALF-KB ','OTHER   '/
C-----------------------------------------------------------------------
C                                       Initialization
      IER = 0
      C1 = 360.D0 / 65536.D0 / 32768.D0
      C2 = 1.0 / 3.6D8
C                                       Add HI images
C                                       Source qualifier
      WRITE (HILINE,1020) D2H(5)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
C                                       Velocity
      CALL ZR8P4 (R4IB8, D2H(11), VALUE)
      WRITE (HILINE,1040) VALUE
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
C                                       Observing band
      ITEMP1 = ZAND (D2H(8), MSKLO)
      IF ((ITEMP1.LT.1) .OR. (ITEMP1.GT.7)) ITEMP1 = 7
      WRITE (HILINE,1050) BAND(ITEMP1)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
C                                       Modified Julian Day
      WRITE (HILINE,1060) D2H(44)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
C                                       Map number
      WRITE (HILINE,1070) D2H(6)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
C                                       Taper functions
      CALL ZR8P4 (R4IB8, D2H(51), VALUE)
      VALUE = VALUE / 10.D0
      CALL ZR8P4 (R4IB8, D2H(53), VALUE1)
      VALUE1 = VALUE1 / 10.D0
      TEMP1 = D2H(55)/10.0
      ITEMP1 = D2H(56)
      IF ((ITEMP1.LT.1) .OR. (ITEMP1.GT.3)) ITEMP1 = 3
      IF (ITEMP1.EQ.1) THEN
         VALUE = 0.D0
         VALUE1 = 0.D0
         TEMP1 = 0.0
         END IF
      WRITE (HILINE,1080) TAPTYP(ITEMP1)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
      WRITE (HILINE,1090) VALUE,VALUE1,TEMP1
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
C                                       U-V Convolution parameters
      CALL ZR8P4 (R4IB8, D2H(57), VALUE)
      VALUE = VALUE / 10.D0
      CALL ZR8P4 (R4IB8, D2H(59), VALUE1)
      VALUE1 = VALUE1 / 10.D0
      ITEMP1 = D2H(61) + 1
      ITEMP2 = D2H(62) + 1
      IF ((ITEMP1.LT.1) .OR. (ITEMP1.GT.6)) ITEMP1 = 6
      IF ((ITEMP2.LT.1) .OR. (ITEMP2.GT.6)) ITEMP2 = 6
      WRITE (HILINE,1100) CONTYP(ITEMP1), VALUE
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
      WRITE (HILINE,1110) CONTYP(ITEMP2), VALUE1
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
C                                       Gridding correction
      CALL ZGTBIT (16, D2H(7), BIT)
      FLAG = .TRUE.
      IF (BIT(11).EQ.0) FLAG = .FALSE.
      WRITE (HILINE,1120) FLAG
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
C                                       User comments
C                                       must undo & redo conversion
      CALL ZILI16 (20, D2H(81), 1, D2H(81))
      CALL ZC8CL (40, 1, D2H(81), COMM)
      CALL CHR2H (40, COMM, 1, D2H(81))
      WRITE (HILINE,1130) COMM
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
C                                       Map type
      WRITE (HILINE,1140) (MAPTYP(BIT(17-I)+1,I), I=1,3)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
C                                       Input bits/pixel
      WRITE (LINE,1150)
      CALL HIAD80 (HLUN, 1, LINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 210
C                                       Clean parameters
      IF (BIT(16).NE.0) THEN
         CALL ZR8P4 (R4IB8, D2H(35), VALUE)
         VALUE = VALUE * C2
         CALL ZR8P4 (R4IB8, D2H(37), VALUE1)
         VALUE1 = VALUE1 * C2
         TEMP1 = D2H(39) / 100.0
         WRITE (HILINE,1160) VALUE, VALUE1, TEMP1
         CALL HIADD (HLUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 210
C                                       Clean product
         ITEMP1 = D2H(8) / 256 + 1
         IF ((ITEMP1.LT.1) .OR. (ITEMP1.GT.5)) ITEMP1 = 1
         I = ITEMP1 - 1
         WRITE (HILINE,1170) D2H(40), I, CLNPR(ITEMP1)
         CALL HIADD (HLUN, HILINE, HBUFF, IERR)
         END IF
      IF (IERR.EQ.0) GO TO 980
C                                       Error in setting up HI cards
 210  WRITE (MSGTXT,1210)
      CALL MSGWRT (6)
C                                       Close history to allow reopen
 980  CALL HICLOS (HLUN, T, HBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('IBM   SQUAL=',I6,12X,' /SOURCE QUALIFIER')
 1040 FORMAT ('IBM   VELOCITY=',E11.5,4X,' /VELOCITY (M/S) OF',
     *   ' FREQ. REF. PIXEL')
 1050 FORMAT ('IBM   BAND=','''',A2,'''',15X,' /OBSERVING BAND')
 1060 FORMAT ('IBM   MJAD=',I6,13X,' /MODIFIED JULIAN DAY NUMBER')
 1070 FORMAT ('IBM   MAPNO=',I6,12X,' /MAP NUMBER')
 1080 FORMAT ('IBM   TAPTYP=','''',A8,'''',7X,' /TAPER TYPE')
 1090 FORMAT ('IBM   TAPMAJ=',E11.5,6X,'TAPMIN=',E11.5,6X,
     *   'TAPPA=',F7.2)
 1100 FORMAT ('IBM   UCONVTYP=','''',A8,'''',6X,'UCONVVAL=',E11.5)
 1110 FORMAT ('IBM   VCONVTYP=','''',A8,'''',6X,'VCONVVAL=',E11.5)
 1120 FORMAT ('IBM   GRIDCORR=',L2,13X,' /GRID CORRECTION APPLIED?')
 1130 FORMAT ('IBM   USERCOMM=''',10A4,''' /USER COMMENTS')
 1140 FORMAT ('IBM   MAPTYPE=',3('''',A8,'''',2X),2X,
     *   ' /MAP AND DATA TYPE')
 1150 FORMAT ('IBM   INBITPIX=16',13X,' /INPUT BITS PER PIXEL')
 1160 FORMAT ('IBM   CLEAN  BMAJ=',E12.4,' BMIN=',E12.4,' BPA=',F7.2)
 1170 FORMAT ('IBM   CLEAN  NITER=',I6,' PRODUCT=',I1,'  / ',A8)
 1210 FORMAT ('TROUBLE WITH HI FILE.  WARNING MESSAGE.')
      END
      SUBROUTINE FITTAP (HLUN, HBUFF, KVOL, IERR)
C-----------------------------------------------------------------------
C  Process FITS type tape header, data, and extension files.
C  Inputs:
C     HLUN   I        Open History file LUN.
C     HBUFF  I(256)   History file work I/O buffer.
C  Outputs:
C     KVOL   I        Disk used
C     IERR   I        Error code, 0=ok.
C-----------------------------------------------------------------------
      INTEGER   HLUN, HBUFF(256), IERR
      INTEGER   ISLOT, SEQ, KVOL, NUMTAB, IROUND, IOP, ITSAVE
      LOGICAL   NODATA, EOF, MORTAB
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      KVOL = KVOL4 + .5
      SEQ = IROUND (SEQ4)
      IOP = 1
      KEEP = 1
C                                       Does header & history using a
C                                       temporary name in catalog.
 10   CALL FITHDR (IOP, KVOL, HLUN, HBUFF, ISLOT, NODATA, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get the data and store in file
      IF (NODATA) THEN
         KEEP = 0
      ELSE
         CALL FITDAT (KVOL, CNO, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Add inputs file to history.
      ITSAVE = CATBLK(KIIMS)
      CATBLK(KIIMS) = SEQ
      CALL HISINP (KVOL, CNO, HLUN, HBUFF)
      CATBLK(KIIMS) = ITSAVE
C                                       Skip any tables.
      IF (DOTABL.LE.0.0) THEN
         CALL MLREOF (FDVEC, TBIND, UNKNWN, TAPBUF, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Standard fits extension records.
      ELSE
         CALL FITRXM (KVOL, HLUN, HBUFF, NUMTAB, EOF, IERR)
         IF (IERR.GT.0) GO TO 999
         MORTAB = IERR.LT.0
         IERR = 0
C                                       Old tables records.
         IF (.NOT.EOF) CALL MLTABL (KVOL, HLUN, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (UNKNWN.GT.0) THEN
            WRITE (MSGTXT,1030) UNKNWN
            CALL MSGWRT (6)
            END IF
C                                       If no data use table name for
C                                       image.
         IF (NODATA) CALL FIXNDT (KVOL, NUMTAB, IERR)
         END IF
C                                       Fill in default names if needed.
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL MLDEF (NAME, CLASS, SEQ)
C                                       Renames to the proper name.
      CALL RNAM (ISLOT, KVOL, IERR)
C                                       Trap case where there were too
C                                       many tables of a given type and
C                                       a dummy file is needed for the
C                                       excess tables.
      IF (MORTAB) THEN
         EOF = .FALSE.
         NODATA = .TRUE.
         IOP = 2
         IF (SEQ.GT.0) SEQ = SEQ + 1
         WRITE (MSGTXT,1050)
         CALL MSGWRT (6)
         GO TO 10
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('WARNING:',I7,' unknown records skipped while reading',
     *   ' tape')
 1050 FORMAT ('Too many tables for a single file, must create another')
      END
      SUBROUTINE FITDAT (KVOL, KNO, IER)
C-----------------------------------------------------------------------
C   FITDAT reads the input data file and scales the data to disk.
C   Inputs:
C      KVOL  I     desired map disk
C   Outputs:
C      IER   I     Error return:  0--> okay
C                                 1--> error condition
C-----------------------------------------------------------------------
      INTEGER   KVOL, KNO, IER
C
      CHARACTER MNAME*48
      INTEGER   BLKS, IERR, IWIN(4), NBKOF1, IOFF, NX, NY, IDEPTH(5),
     *   NBYB, I, INX, INY, IBL, ITEMP, NXY, I3, I3B, I4, I4B, I5, I5B,
     *   I6, I6B, I7, I7B, DLUN, DIND,  NTAPVL, III, L0, L1, L2, NXX,
     *   OUTIND
      INCLUDE 'IMLOD.INC'
      REAL      BUFF(MABFSS), MMAX, MMIN, INBUFR(MABFSS)
      DOUBLE PRECISION    BSC, BZE, DPBUFR(MABFSS/2), DTEMP
      LOGICAL   T, WASBLK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (INBUFR(1), INBUFF(1), DPBUFR(1))
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      DLUN = 16
      MMAX = -1.E20
      MMIN =  -MMAX
      WASBLK = .FALSE.
C                                       Open map file.
      CALL ZPHFIL ('MA', KVOL, KNO, 1, MNAME, IERR)
      CALL ZOPEN (DLUN, DIND, KVOL, MNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) MNAME, IERR
         GO TO 980
         END IF
C                                       Initialize
      IER = 0
      BSC = SCALE
      IF (BSC.EQ.0.0D0) BSC = 1.0D0
      BZE = OFFSET
      NBYB = MABFSS * 2
C                                       Set window parms
      I3B = MAX (1, CATBLK(KINAX+2))
      I4B = MAX (1, CATBLK(KINAX+3))
      I5B = MAX (1, CATBLK(KINAX+4))
      I6B = MAX (1, CATBLK(KINAX+5))
      I7B = MAX (1, CATBLK(KINAX+6))
      CATBLK(KINAX+1) = MAX (1, CATBLK(KINAX+1))
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = CATBLK(KINAX)
      IWIN(4) = CATBLK(KINAX+1)
      NY = IWIN(4)
      NX = IWIN(3)
      INX = CATBLK(KINAX)
      INY = CATBLK(KINAX+1)
C                                       Initialize tape
      NBPIX = TAPEBP
      BLKS = (ABS(NBPIX) / 8)
      NTAPVL = 2880 / BLKS
      IOFF = NTAPVL
      BLKS = BLKS * NX * NY * I3B
      BLKS = BLKS * I4B * I5B * I6B * I7B
      BLKS = (BLKS - 1) / 2880 + 1
      BLKS = BLKS - 1
      IF (IERR.NE.0) GO TO 970
C                                       Test for Kitt Peak "error"
      IF ((IBLNK.EQ.0) .AND. (NBPIX.EQ.8)) ISBLNK = .FALSE.
      DO 200 I7 = 1,I7B
      DO 200 I6 = 1,I6B
      DO 200 I5 = 1,I5B
      DO 200 I4 = 1,I4B
      DO 200 I3 = 1,I3B
C                                       Initialize disk
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, NBKOF1,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         NBKOF1 = NBKOF1 + 1
         CALL MINIT ('WRIT', DLUN, DIND, INX, INY, IWIN, BUFF, NBYB,
     *      NBKOF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR
            GO TO 980
            END IF
C                                       Begin read/write loop
         DO 195 I = 1,NY
C                                       Write a map line
            CALL MDISK ('WRIT', DLUN, DIND, BUFF, OUTIND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR, I
               GO TO 980
               END IF
            NXY = NX
            IBL = 0
C                                       Copy and read until entire map
C                                       row filled.
 55         NXX = MIN (NXY, NTAPVL-IOFF)
C                                       Need more tape values.
               IF (NXX.GT.0) GO TO 60
                  BLKS = BLKS - 1
                  CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
                  IOFF = 0
                  IF (NBPIX.EQ.8) CALL ZI8IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.16) CALL ZI16IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.32) CALL ZI32IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.-32) CALL ZR32RL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.-64) CALL ZR64RL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (IERR.EQ.0) GO TO 55
                     GO TO 970
C                                       INT in: copy convert max/min
 60            IF ((NBPIX.EQ.8) .OR. (NBPIX.EQ.16) .OR. (NBPIX.EQ.32))
     *            THEN
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  IF (ISBLNK) THEN
                     DO 100 III = 1,NXX
                        L1 = L2 + III
                        ITEMP = INBUFF(L0+III)
C                                       Blank pixel found
                        IF (ITEMP.EQ.IBLNK) THEN
                           BUFF(L1) = FBLANK
                           WASBLK = .TRUE.
C                                       scale
                        ELSE
                           BUFF(L1) = BSC * ITEMP + BZE
                           MMIN = MIN (MMIN, BUFF(L1))
                           MMAX = MAX (MMAX, BUFF(L1))
                           END IF
 100                    CONTINUE
                  ELSE
                     DO 115 III = 1,NXX
                        L1 = L2 + III
                        BUFF(L1) = BSC * INBUFF(L0+III) + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
 115                    CONTINUE
                     END IF
                  GO TO 190
C                                       IEEE 64-bit in
               ELSE IF (NBPIX.EQ.-64) THEN
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  DO 150 III = 1,NXX
                     L1 = L2 + III
                     DTEMP = DPBUFR(L0+III)
                     IF (DTEMP.EQ.DBLANK) THEN
                        WASBLK = .TRUE.
                        BUFF(L1) = FBLANK
                     ELSE
                        BUFF(L1) = BSC * DTEMP + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
                        END IF
 150                 CONTINUE
C                                       IEEE 32-bit in
               ELSE
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  DO 160 III = 1,NXX
                     L1 = L2 + III
                     BUFF(L1) = INBUFR(L0+III)
                     IF (BUFF(L1).EQ.FBLANK) THEN
                        WASBLK = .TRUE.
                     ELSE
                        BUFF(L1) = BSC * BUFF(L1) + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
                        END IF
 160                 CONTINUE
                  END IF
C                                       Up the counters
 190           IBL = IBL + NXX
               IOFF = IOFF + NXX
               NXY = NXY - NXX
C                                       loop back if needed to finish
               IF (NXY.GT.0) GO TO 55
 195         CONTINUE
C                                       Flush this plane.
         CALL MDISK ('FINI', DLUN, DIND, BUFF, OUTIND, IERR)
         IF (IERR.NE.0) GO TO 970
 200     CONTINUE
C                                       close files
      CATR(KRDMX) = MMAX
      CATR(KRDMN) = MMIN
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
      CALL MAPCLS ('WRIT', KVOL, KNO, DLUN, DIND, CATBLK, T, BUFF, IERR)
      NCFILE = NCFILE - 1
      GO TO 999
C                                       Error
 970  WRITE (MSGTXT,1970) IERR
 980  CALL MSGWRT (8)
      IF (IERR.EQ.4) THEN
         WRITE (MSGTXT,1980)
         CALL MSGWRT(8)
         END IF
 990  IER = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FITDAT: COULD NOT OPEN MAP ',6A4,' IER=',I4)
 1020 FORMAT ('FITDAT: COULD NOT INITIALIZE DISK FILE.  IER=',I4)
 1030 FORMAT ('FITDAT: COULD NOT WRITE DISK RECORD.  IER=',I3,
     *   ' LINE=',I4)
 1970 FORMAT ('FITDAT: COULD NOT READ INPUT.  IER=',I4)
 1980 FORMAT ('FITDAT: - MAYBE PREMATURE END OF FILE?  CHECK FILE SIZE')
      END
      SUBROUTINE HISINP (KVOL, KCNO, HLUN, HBUFF)
C-----------------------------------------------------------------------
C   Add inputs to the history file.
C   Inputs:
C     KVOL   I         Disk volume number of history file and map.
C     KCNO   I         Catalog number
C     HLUN   I         History file LUN.
C     HBUFF  I(256)    History I/O work buffer.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   KVOL, KCNO, HLUN, HBUFF(256), IERR
      LOGICAL   T
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL HIOPEN (HLUN, KVOL, KCNO, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (HILINE,1000) NAME, CLASS
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (HILINE,1001) CATBLK(KIIMS), NTAPE, KVOL
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (DODISK) THEN
         WRITE (HILINE,1002) INFILE
         CALL HIADD (HLUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      WRITE (HILINE,1003) RLSNAM
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Close history
      CALL HICLOS (HLUN, T, HBUFF, IERR)
      IF (IERR.EQ.0) GO TO 999
C                                       Error.
 980  WRITE (MSGTXT,1980)
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMLOD OUTNAME =''',A12,'''',6X,'OUTCLASS =''',A6,
     *   '''')
 1001 FORMAT ('IMLOD OUTSEQ =',I5,5X,'INTAPE =',I2,5X,'OUTDISK=',I2)
 1002 FORMAT ('IMLOD INFILE = ''',A,'''')
 1003 FORMAT ('IMLOD RELEASE = ''',A7,'''')
 1980 FORMAT ('ERROR IN ADDING TO HI FILE.  WARNING ONLY')
      END
      SUBROUTINE FIXNDT (KVOL, NUMTAB, IERR)
C-----------------------------------------------------------------------
C  This routine will fix up the map header of a tape with no data
C  section, but with at least one extension table.
C  Inputs:
C     KVOL    I    Disk volume number.
C     NUMTAB  I    Number of tables files.  0 produces an error message.
C  Output:
C     IERR    I    Error code, 0=ok.
C-----------------------------------------------------------------------
      INTEGER   KVOL, NUMTAB, IERR
C
      CHARACTER MNAME*48, CHTM12*12, CNAME*12, CCLASS*6, STAT*4
      REAL      BUFF(256)
      INTEGER   LBFSZ, DLUN, DIND, INX, INY, IWIN(4), NBKOF, OUTIND,
     *   JERR, IBUFF(256)
      LOGICAL   T
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IBUFF, BUFF)
      DATA DLUN, INX, INY, IWIN, NBKOF /16,1,1, 4*1, 1/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IF (NUMTAB.LE.0) GO TO 980
C                                       Fix up header.
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), CHTM12)
      IF (CHTM12.EQ.' ') CALL CHR2H (12, EXTNAM, KHIMNO, CATH(KHIMN))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTM12)
      IF (CHTM12(1:6).EQ.' ') CALL CHR2H (6, EXTTYP, KHIMCO,
     *   CATH(KHIMC))
C                                       Coordinates.
      CALL CHR2H (8, 'DUMMY   ', 1, CATH(KHCTP))
      CALL CHR2H (8, 'DUMMY   ', 1, CATH(KHCTP+2))
C                                       Max, min.
      CATR(KRDMX) = 1.0
      CATR(KRDMN) = 0.0
      CATR(KRBLK) = FBLANK
C                                       Open map file.
      CALL ZPHFIL ('MA', KVOL, CNO, 1, MNAME, IERR)
      CALL ZOPEN (DLUN, DIND, KVOL, MNAME, T, T, T, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Write 2x2 image
C                                       To hell with any write errors
      LBFSZ = 256 * 2
      CALL MINIT ('WRIT', DLUN, DIND, INX, INY, IWIN, BUFF, LBFSZ,
     *   NBKOF, JERR)
      IF (JERR.NE.0) GO TO 900
C                                       Write first row
      CALL MDISK ('WRIT', DLUN, DIND, BUFF, OUTIND, JERR)
      IF (JERR.NE.0) GO TO 900
      BUFF(OUTIND) = 1.0
      BUFF(OUTIND+1) = 0.0
C                                       Write last row
      CALL MDISK ('WRIT', DLUN, DIND, BUFF, OUTIND, JERR)
      IF (JERR.NE.0) GO TO 900
      BUFF(OUTIND) = 1.0
      BUFF(OUTIND+1) = 0.0
C                                       Flush buffer
      CALL MDISK ('FINI', DLUN, DIND, BUFF, OUTIND, JERR)
      IF (JERR.NE.0) GO TO 900
C                                       Close
 900  CALL ZCLOSE (DLUN, DIND, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Clear write status
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), CNAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CCLASS)
      STAT = 'CLWR'
      CALL CATDIR ('CSTA', KVOL, CNO, CNAME, CCLASS, CATBLK(KIIMS),
     *   'MA', NLUSER, STAT, IBUFF, JERR)
C                                       Clear in /CFILES/
      NCFILE = NCFILE - 1
      GO TO 999
C                                       Problem
 970  WRITE (MSGTXT,1970)
      GO TO 990
C                                       No tables.
 980  IERR = 1
      WRITE (MSGTXT,1980)
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('NO DATA AND NO TABLES FOUND. DELETING IMAGE.')
 1970 FORMAT ('FITNDT: ERROR WRITING DUMMY IMAGE')
      END
      SUBROUTINE FITHDR (IOP, KVOL, HLUN, HBUFF, ISLOT, NODATA, IERR)
C-----------------------------------------------------------------------
C   FITHDR reads the tape which must be open and positioned at begin.
C   of file) and builds a catalog header and pointers from the
C   tape header records.  After the required fits cards are read a
C   map file with a temporary name is created and the history records
C   are recognized and written to the history file as the other header
C   cards are processed.  The file is later renamed to the correct name.
C   Inputs:
C     IOP    I     Operation code 1=> read tape, 2=>just create dummy
C                  file
C     KVOL   I     Disk volume for cataloged map.
C     HLUN   I     History file logical unit number.
C     HBUFF  I(256)   work buffer.
C   Output:C
C     ISLOT  I     Catalog slot number for new map file.
C     NODATA L     True if tape contains no data section, else false.
C     ERR    I     =0 => ok
C                   other => quit
C-----------------------------------------------------------------------
      CHARACTER LINE*80, HILINE*72, CHTEMP*8
      REAL      PIX11(2)
      INTEGER   IOP, HBUFF(256), ICARD, IE, IST, IERR, ISLOT, IREC, I,
     *   IN, IS, IAX, HLUN, IDEPTH(5), ICEND, KVOL
      LOGICAL   END, F, T, ISHIST, NODATA, DOHI
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize BLANK values flag
C                                       to false.
      NODATA = .FALSE.
      ISBLNK = .FALSE.
      FUCKUP = .FALSE.
      CALL CATCLR (CATBLK)
C                                       See if tape read requested
      IF (IOP.EQ.2) THEN
         CATBLK(KIDIM) = 0
C                                       Initialize header values.
      ELSE
         CALL CATINI (CATBLK)
         SCALE = 1.0D0
         OFFSET = 0.0D0
         ISCALE = 1.0D0
         IZERO = 0.0D0
         CALL RFILL (49, 0.0, PCMATX)
         CALL RFILL (49, 0.0, CDMATX)
         CALL RFILL (49, 0.0, PVMATX)
C                                       Record 1 already read
         CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
C                                       Decode required cards.
         CALL IMREQC (HDRBUF, ICEND, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Make a 2x2 map.
      IF (CATBLK(KIDIM).LE.0) THEN
C                                       No data, must have tables.
         IF (.NOT.STDEXT) GO TO 980
         NODATA = .TRUE.
         ISBLNK = .TRUE.
         CATBLK(KIDIM) = 2
         CATBLK(KINAX) = 2
         CATBLK(KINAX+1) = 2
         END IF
C                                       More defaults.
      DO 10 I = 1,KICTPN
         CATR(KRCRP+I-1) = 0.0
         CATR(KRCIC+I-1) = 1.0
 10      CONTINUE
C                                       Create map with temporary name.
C                                       Map will be renamed later.
      CALL CHR2H (12, 'IMLOD       ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, 'TEMP  ', KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = 0
      CATBLK(KIIMU) = NLUSER
      CALL MCREAT (KVOL, CNO, HBUFF, IERR)
C                                       Blank name back out.
      CALL CHR2H (12, '            ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, '      ', KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = 0
      ISLOT = CNO
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
      NCFILE = 1
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 2
      FVOL(NCFILE) = KVOL
C                                       Create HI file
      CALL HICREA (HLUN, KVOL, CNO, CATBLK, HBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020)
         GO TO 990
         END IF
C                                       Header msg in HI
      LINE = '--------------------------------------------------' //
     *   '------------------'
      CALL HIAD80 (HLUN, 1, LINE, HBUFF, IERR)
      WRITE (LINE,1026)
      IF (IERR.EQ.0) CALL HIAD80 (HLUN, 1, LINE, HBUFF, IERR)
C                                       See if need to parse rest of
C                                       header.
      IF (IOP.NE.2) THEN
         ICARD = ICEND + 1
C                                       Loop until END card found.
         DO 90 IREC = 1,100000000
C                                       Read next record.
            IF (ICARD.GT.36) THEN
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
               ICARD = 1
               END IF
C                                       Parse card, put value in hdr.
            CALL IMPARS (ICARD, HDRBUF, ISHIST, END, IERR)
            IF (END) GO TO 100
C                                       Add to history file.
            IF (IERR.GT.0) THEN
               IST = 80 * ICARD - 79
               CHTEMP = HDRBUF(IST:)
               DOHI = (CHTEMP.EQ.'HISTORY') .OR. (CHTEMP.EQ.'COMMENT')
     *            .OR. (CHTEMP.EQ.' ')
               LINE = HDRBUF(IST:)
               IST = 1
               IF (DOHI) IST = IST + 8
               CALL HIAD80 (HLUN, IST, LINE, HBUFF, IERR)
            ELSE IF (IERR.EQ.-1) THEN
               IS = (ICARD - 1) * 80 + 1
               IE = IS + 79
               CALL PUTCRD (HDRBUF(IS:IE), KVOL, CNO, IERR)
               IF (IERR.GT.1) THEN
                  WRITE (MSGTXT,1080) IERR
                  CALL MSGWRT (7)
                  GO TO 999
                  END IF
               END IF
            ICARD = ICARD + 1
 90         CONTINUE
C                                       Read more cards than we expected
         WRITE (MSGTXT,1090)
         GO TO 990
C                                       End card found.
C                                       Make axis increments non zero
C                                       to help out dumb programs.
 100     IN = KINAX
         IS = KRCIC
         IE = IS + CATBLK(KIDIM) - 1
         DO 200 IAX = IS,IE
            IF ((CATR(IAX).EQ.0.0).AND.(CATBLK(IN).EQ.1))
     *         CATR(IAX) = 1.0
            IN = IN + 1
 200        CONTINUE
         END IF
C                                       End FITS header section in HI
      WRITE (HILINE,1110)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      LINE ='--------------------------------------------------'//
     *   '------------------'
      IF (IERR.EQ.0) CALL HIAD80 (HLUN, 1, LINE, HBUFF, IERR)
C                                       Close history
      CALL HICLOS (HLUN, T, HBUFF, IERR)
C                                       PC -> CROTA
C                                       CD -> CDELT, CROTA
      CALL PCHDR (PCMATX, CDMATX, PVMATX)
C                                       Correct for PDP 11 values.
C                                       set common values
      IF (FUCKUP) THEN
C                                       set up coordinates
         DO 310 I = 3,7
            IDEPTH(I-2) = 1
            IF (I.LE.CATBLK(KIDIM)) THEN
               IDEPTH(I-2) = CATR(KRCRP+I-1) + 0.5
               IDEPTH(I-2) = MAX (1, MIN (IDEPTH(I-2),
     *            CATBLK(KINAX+I-1)))
               END IF
 310        CONTINUE
         LOCNUM = 1
         CALL SETLOC (IDEPTH, F)
C                                       do conversion
         IF ((ABS(POS11(1)-CATD(KDCRV+KLOCL(LOCNUM))).GE.
     *      ABS(0.01*CATR(KRCIC+KLOCL(LOCNUM)))) .OR.
     *      (ABS(POS11(2)-CATD(KDCRV+KLOCM(LOCNUM))).GE.
     *      ABS(0.01*CATR(KRCIC+KLOCM(LOCNUM))))) THEN
            CALL LMPIX (POS11(1), POS11(2), PIX11(1), PIX11(2))
            IF ((ABS(PIX11(1)-CATR(KRCRP+KLOCL(LOCNUM))).GE.
     *         CATBLK(KINAX+KLOCL(LOCNUM))/2) .OR.
     *         (ABS(PIX11(2)-CATR(KRCRP+KLOCM(LOCNUM))).GE.
     *         CATBLK(KINAX+KLOCM(LOCNUM))/2)) THEN
               WRITE (MSGTXT,1310) POS11
               CALL MSGWRT (6)
               WRITE (MSGTXT,1311) PIX11
               CALL MSGWRT (6)
            ELSE
               WRITE (MSGTXT,1320) CATR(KRCRP+KLOCL(LOCNUM)),
     *            CATR(KRCRP+KLOCM(LOCNUM)), PIX11
               CALL MSGWRT (3)
               CATR(KRCRP+KLOCL(LOCNUM)) = PIX11(1)
               CATD(KDCRV+KLOCL(LOCNUM)) = POS11(1)
               CATR(KRCRP+KLOCM(LOCNUM)) = PIX11(2)
               CATD(KDCRV+KLOCM(LOCNUM)) = POS11(2)
               END IF
            END IF
         END IF
      GO TO 999
C                                       No Data, no tables.
 980  WRITE (MSGTXT,1980)
C
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAP CREATE ERROR')
 1020 FORMAT ('HISTORY CREATE ERROR')
 1026 FORMAT ('/Begin "HISTORY" information found in fits tape ',
     *   'header by IMLOD')
 1080 FORMAT ('ERROR',I5,' ADDING KEYWORD TO HEADER FILE')
 1090 FORMAT ('READ MORE THAN 10**8 CARDS WITHOUT FINDING AN END CARD')
 1110 FORMAT ('/END FITS tape header "HISTORY" information')
 1310 FORMAT ('PDP11/70 ERROR: PHASE REF. POS.',2E14.6)
 1311 FORMAT ('GIVES REF. PIXEL',2F9.2,' IGNORED')
 1320 FORMAT ('CORRECTING REF. PIXEL FROM',2F8.2,' TO',2F8.2)
 1980 FORMAT ('THIS FITS FILE HAS NEITHER DATA NOR TABLES')
      END
      SUBROUTINE RNAM (ISLOT, IVOL, IERR)
C-----------------------------------------------------------------------
C   Subroutine to set the catalog slot name, class, and sequence number
C   to match that found in the header.
C   Inputs:
C      ISLOT  I    Catalog slot number to rename.
C      IVOL   I    Disk containing the catalog.
C      COMMON /MAPHDR/
C         CATBLK(K4IMS)   I   Sequence number.
C   Output:
C      IERR   I    0=ok, other is error code from catalog routines.
C-----------------------------------------------------------------------
      CHARACTER XNAMEX*12, CLAS*6, ITYPE*2, STAT*4, KEYWRD(2)*8
      INTEGER   SEQNO, SNO, IERR, ICNO, IVOL, IFIND,
     *   IMAX, IERR2, IMOD, ISLOT, CATLUN, IWORD, NLPR, NWPL,
     *   WBUFF(256), ITEMP, IREC, LOCS(2), KEYTYP(2)
      REAL      RVALUE(4)
      DOUBLE PRECISION DVALUE(2)
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (RVALUE, DVALUE)
      DATA CATLUN /15/
      DATA KEYWRD /'ISCALE','IZERO'/
C-----------------------------------------------------------------------
C                                       Add ISCALE and IZERO to CB file
      IF ((ISCALE.NE.1.0D0) .OR. (IZERO.NE.0.0D0)) THEN
         LOCS(1) = 1
         LOCS(2) = 1 + NWDPDP
         DVALUE(1) = ISCALE
         DVALUE(2) = IZERO
         KEYTYP(1) = 1
         KEYTYP(2) = 1
         CALL CATKEY ('WRIT', IVOL, ISLOT, KEYWRD, 2, LOCS, RVALUE,
     *      KEYTYP, WBUFF, IERR)
         END IF
C                                       Prepare variables.
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), XNAMEX)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAS)
      ITYPE = '  '
      CATBLK(KIIMU) = NLUSER
C                                       Check for default seqno
      SEQNO = CATBLK(KIIMS)
C                                       Is user name unique?
      IF (SEQNO.GT.0) GO TO 10
         ICNO = 1
         SNO = 0
         ITEMP = 0
         CALL CATDIR ('SRCH', ITEMP, ICNO, XNAMEX, CLAS, SNO, ITYPE,
     *      CATBLK(KIIMU), STAT, WBUFF, IERR)
         IF ((IERR.GT.0) .AND. (IERR.NE.5)) GO TO 15
         SEQNO = SNO + 1
         IF (IERR.EQ.5) SEQNO = 1
 10   SNO = SEQNO
      IERR = 0
      ICNO = 1
      ITYPE = '  '
      CALL CATDIR ('SRNN', IVOL, ICNO, XNAMEX, CLAS, SNO, ITYPE,
     *   CATBLK(KIIMU), STAT, WBUFF, IERR)
      IF (IERR.EQ.5) GO TO 20
C                                       No. Print error message.
      IF (IERR.NE.0) GO TO 15
         IERR = 2
         WRITE (MSGTXT,1010) XNAMEX, CLAS, SNO
         CALL MSGWRT (6)
         GO TO 999
C                                       catlg error
 15   CONTINUE
         WRITE (MSGTXT,1015) IERR
         IERR = 3
         CALL MSGWRT (6)
         GO TO 999
C                                       Yes.
 20   CALL H2CHR (2, KHPTYO, CATH(KHPTY), ITYPE)
      WRITE (MSGTXT,1020) XNAMEX, CLAS, SNO, ITYPE, IVOL, ISLOT
      CALL MSGWRT (2)
      CALL CATOPN (IVOL, IFIND, WBUFF, IMAX, IERR)
      IF (IERR.NE.0) GO TO 15
C                                       Catalog entry location:
      NWPL = 10
      NLPR = 256 / NWPL
      IMOD = (ISLOT - 1) / NLPR
      IREC = 2 + IMOD
      IWORD = 1 + NWPL * (ISLOT - NLPR*IMOD - 1)
C                                       Load proper catalog record.
      CALL ZFIO ('READ', CATLUN, IFIND, IREC, WBUFF, IERR)
C                                       Make changes.
      CALL CHR2H (12, XNAMEX, 1, WBUFF(IWORD+5))
      CALL CHR2H (6, CLAS, 13, WBUFF(IWORD+5))
      WBUFF(IWORD+4) = SNO
C                                       Resave record.
      CALL ZFIO ('WRIT', CATLUN, IFIND, IREC, WBUFF, IERR)
      CALL ZCLOSE (CATLUN, IFIND, IERR2)
C                                       Update header with version no.
      CATBLK(KIIMS) = SNO
      CALL CATIO ('UPDT', IVOL, ISLOT, CATBLK, 'REST', WBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('RNAM: DUPLICATE NAME ',A12,'.',A8,'.',I4)
 1015 FORMAT ('RNAM: CATALOG ERROR',I6)
 1020 FORMAT ('RNAM ',A12,'.',A6,'.',I4,' (',A2,')  ON DISK',I2,
     *   ' CNO',I5)
      END
      SUBROUTINE IMREQC (FITBLK, ICARD, IERR)
C-----------------------------------------------------------------------
C   This routine will look for the required cards in a FIT header block
C   SIMPLE, BITPIX, NAXIS, NAXISn, and update a catalog header with the
C   information from these cards.
C   Inputs:
C      FITBLK  C*2880   a block of fit header data.
C   Outputs:
C      ICARD   I        The number of the last card parsed.
C      IERR    I        0=ok, 1=messed up. An error message will
C                                     be printed.
C   COMMON /MAPHDR/ Axis dimension information will be filled in.
C-----------------------------------------------------------------------
      INTEGER   ICARD, IERR
      CHARACTER FITBLK*2880
C
      CHARACTER SYMBOL*8, KL*80
      INTEGER   NPNT, ITYP, NAXIS, ITABNO, IVAL, IKEYWD, I, IAX
      LOGICAL   ISHIST, END
      DOUBLE PRECISION X
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:VFUV.INC'
C-----------------------------------------------------------------------
C                                       Look for SIMPLE=T card
      I = NCT + NKT
      ICARD = 1
      IKEYWD = 1
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETLG (KL, 80, NPNT, ITYP)
      USED(IKEYWD) = USED(IKEYWD) + 1
C                                       Not .TRUE.
      IF (ITYP.NE.1) GO TO 940
C                                       Look for BITPIX.
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      USED(IKEYWD) = USED(IKEYWD) + 1
C                                       Check value of BITPIX
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IF (X.GE.0.) IVAL = X + 0.1
      IF (X.LT.0.) IVAL = X - 0.1
      TAPEBP = IVAL
      IF ((IVAL.NE.8) .AND. (IVAL.NE.16) .AND. (IVAL.NE.32) .AND.
     *   (IVAL.NE.-32) .AND. (IVAL.NE.-64)) GO TO 950
      IF (IVAL.EQ.-64) THEN
         MSGTXT = 'WARNING: 64-bit input stored in 32 bits inside AIPS'
         CALL MSGWRT (6)
         END IF
C                                       Check NAXIS
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 980
      USED(IKEYWD) = USED(IKEYWD) + 1
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      NAXIS = X + .01
C
      IAX = KINAX
      CATBLK(KIDIM) = NAXIS
C                                       Check for invalid no. of axis
C                                       for our header.
      IF (NAXIS.GT.7) GO TO 960
C                                       Check NAXISm
      DO 30 I = 1,NAXIS
         ICARD = ICARD + 1
         IKEYWD = IKEYWD + 1
         NPNT = 1
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT,
     *      KL, SYMBOL, ITABNO, ISHIST, END, IERR)
         IF (IERR.NE.0) GO TO 970
         USED(IKEYWD) = USED(IKEYWD) + 1
         CALL GETNUM (KL, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         CATBLK(IAX) = X + .01
         IAX = IAX + 1
 30      CONTINUE
      IF (CATBLK(KINAX).EQ.0) GO TO 930
C                                       No longer use EXTEND card
      STDEXT = .TRUE.
      GO TO 999
C                                       Probably a UV tape.
 930  WRITE (MSGTXT,1930)
      CALL MSGWRT (8)
C                                       Not SIMPLE FITS tape.
 940  WRITE (MSGTXT,1940)
      GO TO 980
C                                       Invalid bits per pixel value.
 950  WRITE (MSGTXT,1950) IVAL
      GO TO 980
C                                       Invalid number of axis.
 960  WRITE (MSGTXT,1960) NAXIS
      GO TO 980
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) CWORD(IKEYWD), SYMBOL
      GO TO 980
 975  MSGTXT = 'IMREQC: VALUE ERROR PARSING ' // SYMBOL
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1930 FORMAT ('THIS IS PROBABLY A UV DATA FILE THAT MUST BE READ WITH',
     *   ' UVLOD')
 1940 FORMAT ('NOT SIMPLE FITS TAPE. PROGRAM STOPPING.')
 1950 FORMAT ('INVALID BITS PER PIXEL =',I6)
 1960 FORMAT ('INVALID NUMBER OF AXIS =',I6)
 1970 FORMAT ('EXPECTED KEYWORD ',A8,'. FOUND ',A8,'.')
      END
      SUBROUTINE IMPARS (ICARD, FITBLK, ISHIST, END, IERR)
C-----------------------------------------------------------------------
C   IMPARS (parse FITS card) will unpack and interpret a card image
C   from a block of FITS data and put that data into the internal AIPS
C   header.
C   Inputs:
C      ICARD   I         The card number (1-36) in block to interpret.
C      FITBLK  C*2880    A block of FITS header data.
C   Outputs:
C      ISHIST  L         True iff a history card
C      END     L         True if end card found, else false.
C      IERR    I         error code 0=ok. 1=error, -1 => special header
C   COMMON /MAPHDR/
C   COMMON /FITINF/
C-----------------------------------------------------------------------
      INTEGER   ICARD, IERR
      LOGICAL   ISHIST, END
      CHARACTER FITBLK*2880
C
      CHARACTER SYMBOL*8, STR*68, KL*80
      DOUBLE PRECISION X
      REAL      VAL
      LOGICAL   LHIST, FIRST
      INTEGER   KPNTR(58), PNTR, IPOFF, TABNO, NPNT, KT, IL, IVAL,
     *   NCHAR, NBYT, NN, NNSTR, JT, JTRIM, NPNTS
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIT.INC'
      EQUIVALENCE (KPNTR(1), KHOBJ)
      INCLUDE 'INCS:VFIT.INC'
C-----------------------------------------------------------------------
C                                       Find next symbol on the card
C                                       and look for it in the table.
      NPNT = 1
      NN = NKT + NCT
      NNSTR = NCT + 1
C                                       Loop for all possible values
C                                       on an AIPS HISTORY card.
      FIRST = .TRUE.
 10   CALL GETCRD (ICARD, NN, NNSTR, AWORD, FITBLK, NPNT, KL, SYMBOL,
     *    TABNO, LHIST, END, IERR)
      IF (END) GO TO 999
      IF (FIRST) ISHIST = LHIST
      FIRST = .FALSE.
      IF (ISHIST) THEN
         IF (XERR4.GT.1.5) GO TO 999
         IF (IERR.EQ.1) GO TO 10
         IF ((USED(TABNO).GT.0) .AND. (IERR.EQ.0)) GO TO 10
         END IF
      IF ((IERR.EQ.1) .AND. ((SYMBOL(:2).EQ.'CD') .OR.
     *   (SYMBOL(:2).EQ.'PC') .OR. (SYMBOL(:2).EQ.'PV')))
     *   CALL PCCARD (0, KL, PCMATX, CDMATX, PVMATX)
      IF (IERR.NE.0) GO TO 999
      IF (.NOT.ISHIST) USED(TABNO) = USED(TABNO) + 1
C                                       Header pointer for this
C                                       keyword, number bytes and
C                                       offset position from pointer
      PNTR = MOD (APOINT(TABNO), 1000)
      IPOFF = PNTR / 100
      PNTR = MOD (PNTR, 100)
      IF (PNTR.GT.0) PNTR = KPNTR(PNTR)
      NBYT = APOINT(TABNO) / 1000
C                                       Type value of keyword
C                                       1=LOGICAL
C                                       2=NUMBER
C                                       3=STRING
      KT = ATYPE(TABNO)
C                                       Logical value
      NPNTS = NPNT
      IF (KT.EQ.1) THEN
         CALL GETLG (KL, 80, NPNT, IL)
C                                       Illegal logical value.
         IF (IL.LT.0) THEN
            MSGTXT = 'LOGICAL VARIABLE HAS ILLEGAL VALUE: ' // SYMBOL
            GO TO 990
            END IF
C                                       Handle normal logical cases.
         CATBLK(PNTR+IPOFF) = IL
C                                       Number
      ELSE IF (KT.EQ.2) THEN
         CALL GETNUM (KL, 80, NPNT, X)
C                                       special parse for EQUINOX
         IF (X.EQ.DBLANK) THEN
            IF ((AWORD(TABNO).EQ.'EQUINOX') .OR.
     *         (AWORD(TABNO).EQ.'EPOCH')) THEN
               NPNT = NPNTS
               CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
               IF (INDEX(STR,'1950').GT.0) THEN
                  X = 1950.0D0
               ELSE IF (INDEX(STR,'2000').GT.0) THEN
                  X = 2000.0D0
                  END IF
               END IF
            END IF
         IF (X.EQ.DBLANK) GO TO 975
C                                       Check for number special cases.
C                                       Blank pixel value.
         IF (AWORD(TABNO).EQ.'BLANK') THEN
            IF (X.EQ.-2147483648.0D0) THEN
               IBLNK = -2147483647 - 1
            ELSE
               IBLNK = X
               END IF
            ISBLNK = .TRUE.
C                                       PDP 11 Stuff
         ELSE IF ((AWORD(TABNO).EQ.'OPHRAE11') .OR.
     *      (AWORD(TABNO).EQ.'OPHDCE11')) THEN
            POS11(IPOFF) = X
            FUCKUP = .TRUE.
C                                       Handle normal cases. Put value
C                                       into proper header slot.
C                                       2-byte integer
         ELSE IF (NBYT.EQ.2) THEN
            IVAL = X + SIGN (0.5D0, X)
            IF (PNTR.GT.0) THEN
               CATBLK(PNTR+IPOFF) = IVAL
            ELSE
               IF (AWORD(TABNO).EQ.'BITPIX') TAPEBP = IVAL
               IF (AWORD(TABNO).EQ.'TABLES') TABLES = IVAL
               END IF
C                                       4-byte real
         ELSE IF (NBYT.EQ.4) THEN
            IF ((AWORD(TABNO).EQ.'HISTORY') .AND. (X.GT.1.0E30))
     *         X = 1.0E30
            VAL = X
            IF (PNTR.GT.0) CATR(PNTR+IPOFF) = VAL
C                                       8-byte real
         ELSE IF (NBYT.EQ.8) THEN
            IF (PNTR.GT.0) THEN
               CATD(PNTR+IPOFF) = X
            ELSE
               IF (AWORD(TABNO).EQ.'BSCALE') SCALE = X
               IF (AWORD(TABNO).EQ.'ISCALE') ISCALE = X
               IF (AWORD(TABNO).EQ.'BZERO') OFFSET = X
               IF (AWORD(TABNO).EQ.'IZERO') IZERO = X
               END IF
            END IF
C                                       String
      ELSE IF (KT.EQ.3) THEN
         CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
C                                       Dates are special
         IF (AWORD(TABNO)(:4).EQ.'DATE') THEN
            CALL DATFST ('F2L', STR)
            NCHAR = 8
            END IF
         NCHAR = MIN (NBYT, NCHAR)
C                                       Start string on integer boundary
C                                       IMCLASS
         IF (AWORD(TABNO).EQ.'IMCLASS') THEN
            IPOFF = NBYT * IPOFF + 1
            CALL CHFILL (NBYT, HBLANK, IPOFF, CATH(PNTR))
            JT = JTRIM (STR(:NCHAR))
            CALL CHR2H (NCHAR, STR, IPOFF, CATH(PNTR))
C                                       Start string on real boundary.
         ELSE
            IPOFF = (NBYT / 4) * IPOFF
            CALL CHFILL (NBYT, HBLANK, 1, CATH(PNTR+IPOFF))
            JT = JTRIM (STR(:NCHAR))
            CALL CHR2H (NCHAR, STR, 1, CATH(PNTR+IPOFF))
            END IF
         END IF
C                                       If this is a history card, look
C                                       for more values.
      IF (ISHIST) GO TO 10
      GO TO 999
C
 975  MSGTXT = 'IMPARS: NUMBER VALUE ERROR ON ' // SYMBOL
C                                       Error message
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
      END
      SUBROUTINE MLDEF (NAMEX, CLASSX, SEQ)
C-----------------------------------------------------------------------
C   MLDEF fills the image name (name,class,seq) with default values.
C   This subroutine is used with IMLOD.
C   Inputs/Output:
C      NAMEX    C*12  Input image name
C      CLASSX   C*6   Input image class
C      SEQ      I     Input image Sequence
C   Outputs:
C      CATBLK(KIIMS) I     Image sequence
C-----------------------------------------------------------------------
      CHARACTER DNAME*12, DCLASS*6, NAMEX*12, CLASSX*6,
     *   NONE*8, STOKES*8, WTYP(5)*4,
     *   STOK(5)*2, STOK2(5)*4, CHTM12*12
      INTEGER   SEQ, STNUM, NAX, I, J, DSEQ
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA NONE, STOKES /'NONE    ','STOKES  '/
      DATA WTYP /'MAP ','CMP ','RES ','PNT ','CLN '/
      DATA STOK /'I ','I ','Q ','U ','V '/
      DATA STOK2 /'PPOL','FPOL','PANG','SPIX','OPTD'/
C-----------------------------------------------------------------------
C                                       check type
      IF ((CATBLK(KITYP).LE.1) .OR. (CATBLK(KITYP).GT.4))
     *   CATBLK(KITYP) = 1
C                                       name default
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), DNAME)
      IF (DNAME.EQ.' ') THEN
         CALL H2CHR (8, 1, CATH(KHOBJ), CHTM12)
         IF (CHTM12(1:8).EQ.' ') CALL CHR2H (8, NONE, 1, CATH(KHOBJ))
         CALL H2CHR (8, 1, CATH(KHOBJ), DNAME(1:8))
         DNAME(9:12) = ' '
         END IF
C                                       class default
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), DCLASS)
C                                       Stokes value 1st char
      IF (DCLASS.EQ.' ') THEN
         STNUM = 2
         NAX = CATBLK(KIDIM)
         DO 25 I = 1,NAX
            J = (I-1)*2 + KHCTP
            CALL H2CHR (8, 1, CATH(J), CHTM12)
            IF (STOKES.EQ.CHTM12(1:8)) STNUM = CATD(KDCRV+I-1) + 1.5
 25         CONTINUE
C                                       clean type : 2-4 chars
         IF (STNUM.LE.5) THEN
            J = CATBLK(KITYP)
            IF ((J.EQ.1) .AND. (CATBLK(KINIT).GT.0)) J = 5
            IF (STNUM.EQ.1) DCLASS = STOK(STNUM)(1:1) // 'BEM  '
            IF (STNUM.NE.1) DCLASS = STOK(STNUM)(1:1) // WTYP(J)(1:3)
     *         // '  '
         ELSE
C                                       Special "Stokes" values
            DCLASS = STOK2(STNUM-5)(1:4) // '  '
            END IF
         END IF
C                                       sequence number
C                                       fill in cat block
      DSEQ = CATBLK(KIIMS)
      CATBLK(KIIMS) = SEQ
      CALL MAKOUT (DNAME, DCLASS, DSEQ, DCLASS, NAMEX, CLASSX,
     *   CATBLK(KIIMS))
      CALL CHR2H (12, NAMEX, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASSX, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
C
 999  RETURN
      END
      SUBROUTINE MLTAPE (NMPSKP, IERR)
C-----------------------------------------------------------------------
C   MLTAPE sets up for TAPIO and opens the input for IMLOD.
C   Inputs:  NMPSKP      I    Number files to skip
C   Outputs: IERR        I    Error return
C                             0--> okay,  1--> error
C   Uses and build special common /MLTAPE/
C-----------------------------------------------------------------------
      INTEGER   NMPSKP, IERR, IC
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Buffer size.
      FDVEC(3) = (29184 * NBITWD) / 8
C                                       Logical record size (FITS)
      FDVEC(2) = 2880
C                                       Disk output.
      IF (DODISK) THEN
         CALL CHR2H (48, INFILE, 1, FDVEC(7))
         WRITE (MSGTXT,1000) INFILE
         CALL MSGWRT (2)
         FDVEC(1) = 25
         NTAPE = 1
         FDVEC(5) = NTAPE
C                                       Tape input
      ELSE
         NTAPE = NTAPE4 + 0.5
         IF (NTAPE.LE.0) NTAPE = 1
         FDVEC(1) = 129 - NTAPE
         FDVEC(5) = NTAPE
         FDVEC(6) = 10
         WRITE (MSGTXT,1020) NTAPE
         CALL MSGWRT (2)
         END IF
C                                       Open tape
      CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1025) IERR
         GO TO 980
         END IF
C                                       Advance files
      IF (.NOT.DODISK) THEN
         IC = NMPSKP
         IF (IC.GT.0) THEN
            WRITE (MSGTXT,1030) IC
            CALL MSGWRT (2)
            CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), IC, IERR)
            WRITE (MSGTXT,1031) IERR, 'ADVF'
C                                       Back files
         ELSE IF (IC.LT.0) THEN
            IC = -IC
            WRITE (MSGTXT,1035) IC
            IC = IC + 1
            CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), IC, IERR)
            WRITE (MSGTXT,1031) IERR, 'BAKF'
         ELSE
            MSGTXT = 'Reading tape at its current position'
            CALL MSGWRT (2)
            END IF
         END IF
      IF (IERR.EQ.0) GO TO 999
C                                       Error returns
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Reading from disk file: ',A)
 1020 FORMAT ('Reading tape drive number ',I3)
 1025 FORMAT ('MLTAPE: COULD NOT OPEN TAPE.  IER=',I7)
 1030 FORMAT ('ADVANCING TAPE BY',I4,' FILES')
 1031 FORMAT ('ERROR',I4,' ON OPERATION ',A4)
 1035 FORMAT ('Moving tape backwards by',I4,' files')
      END
      SUBROUTINE MLTABL (VOL, HLUN, HBUFF, IRET)
C-----------------------------------------------------------------------
C   MLTABL processes records following the normal FITS image.  If
C   TABLES <= 0, it simply counts the number of such records.  Else,
C   it parses through the Table records creating the appropriate
C   extension files and adding the table header cards to the
C   history file.
C   Inputs:  VOL    I         Output disk volume #
C            HLUN   I         LUN of open history file
C   In/Out:  HBUFF  I(256)    HI work buffer
C   Output:  IRET   I         Error code: 0 => ok, 8 => some error
C-----------------------------------------------------------------------
      INTEGER   VOL, HLUN, HBUFF(256), IRET
C
      CHARACTER ISTR*80, SYM*8, CARD*80, CTYPES(2)*4, LLCHAR*4,
     *   SYMS(15)*8, CHTM12*12, TABNAM*8, TTYPE(10)*8
      INTEGER   TABCNT, IRNO, IERR, TERR, TABVER, TABWID, TABCRD, NC,
     *   NPNT, ITYP, NSYMS, NCHAR, ITAB, NTYPES, IT, LUN, INC,
     *   BUFFER(768), IP, I, IREC, J, NDIM, JJ, IST
      LOGICAL   HISERR, EQUAL, NODATA, T, ISHIS
      REAL      RDATA(10), XINC
      DOUBLE PRECISION    X
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA NSYMS, NTYPES, LUN /15, 2, 28/
      DATA CTYPES,     LLCHAR
     *   /' CCC','  CC','LL  '/
      DATA SYMS /'TTYPE1  ', 'TTYPE2  ', 'TTYPE3  ', 'TTYPE4  ',
     *   'TTYPE5  ', 'TTYPE6  ', 'TTYPE7  ', 'TTYPE8  ',
     *   'TTYPE9  ', 'TTYPE10 ', 'TABNAME ', 'TABVER  ',
     *   'TABCOUNT', 'TABWIDTH', 'TABCARDS'/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL HIOPEN (HLUN, VOL, CNO, HBUFF, IERR)
      IERR = 0
      TERR = 0
      IRET = 0
      IF (TABLES.LE.0) GO TO 900
      HISERR = .FALSE.
      IRET = 8
C                                       Loop over tables
      NC = 0
      DO 200 ITAB = 1,TABLES
         WRITE (CARD,1000) ITAB
         IF (.NOT.HISERR) CALL HIAD80 (HLUN, 1, CARD, HBUFF, IERR)
         IF (IERR.NE.0) HISERR = .TRUE.
C                                       Init table parm values
         TABVER = 0
         TABCNT = 0
         TABWID = 0
         TABCRD = 0
         TABNAM = '        '
         DO 20 I = 1,10
            TTYPE(I) = '        '
 20         CONTINUE
C                                       Read and parse header
         DO 90 IREC = 1,100
            IF (IREC.NE.1) THEN
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
               IF (TERR.NE.0) THEN
                  WRITE (MSGTXT,1020) TERR
                  GO TO 890
                  END IF
               NC = 0
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
               END IF
C                                       card loop
 50         NC = NC + 1
            IF (NC.LE.36) THEN
C                                       card to history
               INC = (NC-1) * 80 + 1
               CARD = HDRBUF(INC:)
               IF (.NOT.HISERR) THEN
                  ISHIS = CARD(1:8).EQ.'HISTORY'
                  IF (.NOT.ISHIS) ISHIS = CARD(1:8).EQ.'COMMENT'
                  IF (.NOT.ISHIS) ISHIS = CARD(1:8).EQ.' '
                  IST = 1
                  IF (ISHIS) IST = 9
                  IF (CARD(1:4).NE.'END ') CALL HIAD80 (HLUN, IST, CARD,
     *               HBUFF, IERR)
                  HISERR = IERR.NE.0
                  END IF
C                                       Parse
               NPNT = 1
               CALL GETSYM (CARD, NPNT, SYM, ITYP)
               IF (SYM.EQ.'END ') GO TO 100
C                                       only keyword = value accepted
               IF (ITYP.NE.0) GO TO 50
               DO 60 I = 1,NSYMS
                  IF (SYM.EQ.SYMS(I)) GO TO 70
 60               CONTINUE
               GO TO 50
C                                       Numeric keywords
 70            IF (I.GT.11) THEN
                  CALL GETNUM (CARD, 80, NPNT, X)
                  IF (X.EQ.DBLANK) GO TO 880
                  IF (I.EQ.12) TABVER = X + 0.01
                  IF (I.EQ.13) TABCNT = X + 0.01
                  IF (I.EQ.14) TABWID = X + 0.01
                  IF (I.EQ.15) TABCRD = X + 0.01
C                                       Got a string variable
               ELSE
                  CALL GETSTR (CARD, 80, 68, NPNT, ISTR, NCHAR)
                  NCHAR = MIN (NCHAR, 8)
                  IF (I.EQ.11) TABNAM = ISTR(1:NCHAR)
                  IF (I.LT.11) TTYPE(I) = ISTR(1:NCHAR)
                  END IF
               GO TO 50
               END IF
 90         CONTINUE
         WRITE (MSGTXT,1090) ITAB
         GO TO 890
C                                       END card found
C                                       null table
 100     IF ((TABCNT.LE.0) .OR. (TABWID.LE.0)) THEN
            WRITE (MSGTXT,1100) ITAB
            CALL MSGWRT (6)
            WRITE (MSGTXT,1170) ITAB
            CALL MSGWRT (2)
            IF (.NOT.HISERR) THEN
               CALL HIAD80 (HLUN, 1, MSGTXT, HBUFF, IERR)
               HISERR = IERR.NE.0
               END IF
            GO TO 200
            END IF
C                                       illegal format
         IF ((TABCRD.LE.0) .OR. (TABCRD.GT.40)) THEN
            WRITE (MSGTXT,1105) TABCRD, ITAB
            GO TO 890
            END IF
C                                       A recognized type?
         NODATA = .TRUE.
         IF (TABNAM(1:4).NE.'AIPS') GO TO 125
            DO 115 IT = 1,NTYPES
               IF (CTYPES(IT).EQ.TABNAM(5:8)) GO TO 120
 115           CONTINUE
            GO TO 125
C                                       Yes: do it - CC files only
 120     IF ((IT.EQ.1) .OR. (IT.EQ.2)) THEN
            NODATA = .FALSE.
C                                       Set correction for old CC
            XINC = 0.0
            IF (IT.EQ.2) THEN
               NDIM = CATBLK(KIDIM)
               DO 122 I = 1,NDIM
                  J = (I-1) * 2 + KHCTP
                  CALL H2CHR (4, 1, CATH(J), CHTM12)
                  EQUAL = LLCHAR(1:4).EQ.CHTM12(1:4)
                  IF (EQUAL) XINC = CATR(KRCIC+I-1)
 122              CONTINUE
               WRITE (MSGTXT,1122)
               CALL MSGWRT (2)
               END IF
            CALL CCINI (LUN, TABWID, VOL, CNO, TABVER, CATBLK, BUFFER,
     *         IERR)
            IF (IERR.NE.0) THEN
               NODATA = .TRUE.
               WRITE (MSGTXT,1124) IERR
               CALL MSGWRT (7)
               END IF
            END IF
 125     IP = TABCRD
         DO 160 IRNO = 1,TABCNT
            DO 150 J = 1,TABWID
               IP = IP + 1
               IF (IP.GT.TABCRD) THEN
                  NC = NC + 1
C                                       read a record
                  IF (NC.GT.36) THEN
                     NC = 1
                     CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
                     CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
                     IF (TERR.NE.0) THEN
                        WRITE (MSGTXT,1020) TERR
                        GO TO 890
                        END IF
                     END IF
C                                       spread new card
                  IP = 1
                  IF (.NOT.NODATA) THEN
                     INC = (NC-1) * 80 + 1
                     CARD = HDRBUF(INC:)
                     NPNT = 1
                     END IF
                  END IF
               IF (.NOT.NODATA) THEN
                  CALL GETNUM (CARD, 80, NPNT, X)
                  IF (X.EQ.DBLANK) GO TO 880
                  JJ = J
C                                       format correction
                  IF (IT.NE.1) THEN
                     IF (J.EQ.1) X = X + XINC
                     IF (J.LE.3) JJ = MOD (J, 3) + 1
                     END IF
                  RDATA(JJ) = X
                  END IF
 150           CONTINUE
            IF (.NOT.NODATA) THEN
               CALL TABIO ('WRIT', 0, IRNO, RDATA, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
 160        CONTINUE
         IF (.NOT.NODATA) THEN
            CALL TABIO ('CLOS', 0, IRNO, RDATA, BUFFER, IERR)
            WRITE (MSGTXT,1160) 'CC', TABVER
            CALL MSGWRT (2)
C                                       Data ignored
         ELSE
            WRITE (MSGTXT,1170) ITAB
            CALL MSGWRT (2)
            IF (.NOT.HISERR) THEN
               CALL HIAD80 (HLUN, 1, MSGTXT, HBUFF, IERR)
               HISERR = IERR.NE.0
               END IF
            END IF
 200     CONTINUE
      IRET = 0
      GO TO 900
C
 880  MSGTXT = 'MLTABL: RANGE ERROR PARSING HEADER'
      IRET = 1
C
 890  CALL MSGWRT (8)
C                                       Read rest of tape
 900  IF (TERR.NE.4) CALL MLREOF (FDVEC, TBIND, UNKNWN, TAPBUF, IERR)
      IF (IERR.NE.0) IRET = 6
C                                       Close history
      CALL HICLOS (HLUN, T, HBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMLOD  / HEADER FOR TABLE',I7)
 1020 FORMAT ('MLTABL: TAPE IO ERROR',I7)
 1090 FORMAT ('MLTABL: NO END TO TABLE HEADER #',I7)
 1100 FORMAT ('TABLE',I7,' HAS NO DATA')
 1105 FORMAT ('MLTABL: TABCARDS=',I7,' ILLEGAL')
 1122 FORMAT ('Correcting old CC format X positions')
 1124 FORMAT ('MLTABL: UNABLE TO CREATE EXTENSION FILE',I7)
 1160 FORMAT ('Extension file type ',A2,' version',I4,' written')
 1170 FORMAT ('IMLOD / table',I7,' skipped')
      END
      SUBROUTINE FITRXM (IVOL, HLUN, HBUFF, NUMTAB, EOF, IERR)
C-----------------------------------------------------------------------
C  This routine will read all fits extension files associated with a map
C  and process the ones it recognizes (XTENSION = 'TABLES' as of now).
C  Inputs:
C     IVOL    I       Disk volume number of map and ext files.
C     HLUN    I       History file LUN to be opened
C     HBUFF   I(256)  History file I/O buffer.
C  Outputs:
C     NUMTAB  I       Number of extension files found.
C     EOF     L       An end of file was read during processing.
C     IERR    I       Error code. 0=ok. >0 => Error
C                     -1 => too many tables for one output file.
C-----------------------------------------------------------------------
      INTEGER   IVOL, HLUN, HBUFF(*), NUMTAB, IERR
      LOGICAL   EOF
C
      INTEGER   MXTBKW, MAXTAB
C                                       MXTBKW=max. no. table keywords
      PARAMETER (MXTBKW = 1000)
C                                       MAXTAB = max number of tables.
      PARAMETER (MAXTAB=46000)
C
      CHARACTER KEYWRD(MXTBKW)*8, KEYCHR(MXTBKW)*8, TABLE*8, TAB3D(3)*8
      LOGICAL   T
      DOUBLE PRECISION NBITS, AXCNT, KEYVAL(MXTBKW), KEYD
      REAL      KEYR(2)
      HOLLERITH KEYH(2)
      INTEGER   I, II, ICARD, INBLK, KEYTYP(MXTBKW), IVER, TABLUN,
     *   SRTORD, DATP(128,2), BUFFER(512), NUMKEY, KEYI, JERR, IKEY,
     *   KEYLOC(MXTBKW), KEYV(2*MXTBKW), LENKEY(5), JT, JTRIM,
     *   CATSAV(256)
      LOGICAL   EXTEN, KEYL, DOHDR
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (KEYL, KEYI, KEYH, KEYR, KEYD)
      DATA TABLE, TAB3D /'TABLE', 'BINTABLE', 'A3DTABLE', '3D TABLE'/
      DATA TABLUN /29/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Length of keyword values
      LENKEY(1) = NWDPDP
      LENKEY(2) = 1
      LENKEY(3) = 2
      LENKEY(4) = 1
      LENKEY(5) = 1
C                                       Open history
      CALL HIOPEN (HLUN, IVOL, CNO, HBUFF, JERR)
      NUMTAB = 0
C                                       Loop for all FITS extensions.
      DO 200 I = 1,1000000
C                                       Process all required FITS table
C                                       cards.
         CALL EXTREQ (FDVEC, TBIND, TAPBUF, HDRBUF, ICARD, EXTEN, EOF,
     *      UNKNWN, IERR)
         IF (EOF) GO TO 900
         IF ((IERR.NE.0) .OR. (.NOT.EXTEN)) GO TO 900
C                                       Calculate no. of data blocks.
         AXCNT = 1.0D0
         DO 20 II = 1,NAXIS
            AXCNT = AXCNT * NAXISI(II)
 20         CONTINUE
         AXCNT = AXCNT + PCOUNT
         NBITS = ABS (BITPIX) * GCOUNT * AXCNT
         INBLK = INT ((NBITS + 23039.0D0) / 23040.0D0)
         DOHDR = .FALSE.
C                                       See if we have an ASCII table.
         IF (EXTTYP.EQ.TABLE) THEN
            IF (NAXISI(1).GT.2880) THEN
               WRITE (MSGTXT,1020) NAXISI(1)
               CALL MSGWRT (6)
               GO TO 100
               END IF
C                                       initialize default values.
            CALL SETDEF
C                                       Process table file header.
            NUMKEY = MXTBKW
            CALL TABHDR (FDVEC, TBIND, ICARD, HLUN, HBUFF, 0, NUMKEY,
     *         KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, HDRBUF, IERR)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.NE.0) GO TO 900
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
            SRTORD = 0
            IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD, KEYVAL,
     *         KEYTYP, KEYCHR, SRTORD)
C                                       Create and initialize the table
C                                       header with data in common.
            IVER = 0
            CALL MAKTAB (SRTORD, IVOL, CNO, IVER, CATBLK, TABLUN, DATP,
     *         BUFFER, JERR)
            IF (JERR.GT.0) GO TO 110
C                                       Trap filling up table type.
            EOF = IVER.GT.MAXTAB
C                                       Prepare keywords
            IKEY = 1
            DO 30 II = 1,NUMKEY
               KEYLOC(II) = IKEY
               IF (KEYTYP(II).EQ.2) KEYTYP(II) = 1
               IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
               IF (KEYTYP(II).EQ.3) THEN
                  JT = JTRIM (KEYCHR(II))
                  CALL CHR2H (8, KEYCHR(II), 1, KEYH)
                  END IF
               IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
               IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
               CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
               IKEY = IKEY + LENKEY(KEYTYP(II))
 30            CONTINUE
C                                       Write keywords.
            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *         KEYV, KEYTYP, JERR)
            IF (JERR.NE.0) THEN
               CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, JERR)
               GO TO 110
               END IF
C                                       Read the data from tape and
C                                       write to the table disk file.
            CALL RWTAB (FDVEC, TBIND, DATP, NAXISI, BUFFER, TAPBUF,
     *         IERR)
            IF (IERR.NE.0) GO TO 900
            KEEP = MAX (KEEP, 1)
C                                       See if we have a 3-D table.
         ELSE IF ((EXTTYP.EQ.TAB3D(1)) .OR. (EXTTYP.EQ.TAB3D(2)) .OR.
     *      (EXTTYP.EQ.TAB3D(3))) THEN
C                                       initialize default values.
            CALL SETDEF
C                                       Process table file header.
            NUMKEY = MXTBKW
            CALL TABHDR (FDVEC, TBIND, ICARD, HLUN, HBUFF, 1, NUMKEY,
     *         KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, HDRBUF, IERR)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.NE.0) GO TO 900
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
            SRTORD = 0
            IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD, KEYVAL,
     *         KEYTYP, KEYCHR, SRTORD)
C                                       Create and initialize the table
C                                       header with data in common.
            IVER = 0
            CALL MAKTAB (SRTORD, IVOL, CNO, IVER, CATBLK, TABLUN, DATP,
     *         BUFFER, JERR)
            IF (JERR.GT.0) GO TO 110
C                                       Trap filling up table type.
            EOF = IVER.GT.MAXTAB
C                                       Prepare keywords
            IKEY = 1
            DO 80 II = 1,NUMKEY
               KEYLOC(II) = IKEY
               IF (KEYTYP(II).EQ.2) KEYTYP(II) = 1
               IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
               IF (KEYTYP(II).EQ.3) THEN
                  JT = JTRIM (KEYCHR(II))
                  CALL CHR2H (8, KEYCHR(II), 1, KEYH)
                  END IF
               IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
               IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
               CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
               IKEY = IKEY + LENKEY(KEYTYP(II))
 80            CONTINUE
C                                       Write keywords.
            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *         KEYV, KEYTYP, JERR)
            IF (JERR.NE.0) THEN
               CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, JERR)
               GO TO 110
               END IF
C                                       Read the data from tape and
C                                       write to the table disk file.
            CALL R3DTAB (FDVEC, TBIND, DATP, NAXISI, BUFFER, TAPBUF,
     *         IERR)
            IF (IERR.NE.0) GO TO 900
            KEEP = MAX (KEEP, 1)
C                                       IMAGE extension
         ELSE IF (EXTTYP.EQ.'IMAGE') THEN
            CALL COPY (256, CATBLK, CATSAV)
            CALL RIMAGE (IVOL, HLUN, HBUFF, ICARD, IERR)
            CALL COPY (256, CATSAV, CATBLK)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.GT.0) GO TO 900
            IF (KEEP.LE.0) KEEP = -1
         ELSE
            GO TO 100
            END IF
         NUMTAB = NUMTAB + 1
         GO TO 190
C                                       Skip unknown extension file.
C                                       read rest header code
 100     DOHDR = .TRUE.
C                                       else header already read
 110     CALL SKPEXT (DOHDR, FDVEC, TBIND, HLUN, ICARD, INBLK, HBUFF,
     *      TAPBUF, HDRBUF, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Quit if filled up tables.
 190     IF (EOF) GO TO 900
C                                       Change /CFILES/ not to destroy
C                                       on ERROR
         FRW(1) = 1
 200     CONTINUE
C                                       Shouldn't get here.
      WRITE (MSGTXT,1200)
      CALL MSGWRT (6)
C                                       Close history
 900  CALL HICLOS (HLUN, T, HBUFF, JERR)
C                                       Trap too many tables
      IF ((IVER.GT.MAXTAB) .AND. (IERR.EQ.0)) IERR = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('FITRXM: ASCII TABLE ROW LENGTH',I7,' TOO LONG FOR ME')
 1200 FORMAT ('MORE THAN 32000 EXTENSION FILES. SOME NOT PROCESSED.')
      END
      SUBROUTINE IMQUIT
C-----------------------------------------------------------------------
C  Print out map header summary.
C-----------------------------------------------------------------------
      INCLUDE 'IMLOD.INC'
C-----------------------------------------------------------------------
C                                       display header
      CALL LSTHDR (CATBLK, CATH, CATR, CATD, ISCALE, IZERO)
C
 999  RETURN
      END
      SUBROUTINE RIMAGE (IVOL, HLUN, HBUFF, ICARD, IERR)
C-----------------------------------------------------------------------
C   RIMAGE reads an IMAGE extension.  It creates and fills that image,
C   makes a history file using the current contents of the master image,
C   and then closes the map file.
C   Inputs
C      IVOL    I      Disk to use
C      HLUN    I      HI file open in master
C   In/out:
C      HBUFF   I(*)   HI bufffer
C   Output:
C      IERR    I      error code
C-----------------------------------------------------------------------
      INTEGER   IVOL, HLUN, HBUFF(*), ICARD, IERR
C
      INTEGER   LCNO, LHBUFF(256), LHLUN, HERR, SEQ, I, J, IREC,
     *   IST, IS, IE, IN, IAX
      CHARACTER LINE*80, CHTEMP*8, NAMEXT*16
      LOGICAL   NODATA, ISHIST, END, DOHI
      INCLUDE 'IMLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LHLUN /29/
C-----------------------------------------------------------------------
      IF ((GCOUNT.GT.1) .OR. (PCOUNT.GT.0)) THEN
         WRITE (MSGTXT,1010) PCOUNT, GCOUNT
         CALL MSGWRT (8)
         IERR = -1
         GO TO 999
         END IF
C                                       close input HI file
      CALL HICLOS (HLUN, .TRUE., HBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSING MASTER HI FILE'
         CALL MSGWRT (6)
         END IF
C                                       revise header for new image
      CATBLK(KIDIM) = NAXIS
      CALL COPY (KICTPN, NAXISI, CATBLK(KINAX))
      J = NAXISI(1)
      DO 10 I = 2,NAXIS
         J = J * NAXISI(I)
 10      CONTINUE
      IF (NAXIS.EQ.0) J = 0
      NODATA = J.LE.0
C                                       no extensions here yet
      CALL CATCLR (CATBLK)
C                                       Create map with temporary name.
C                                       Map will be renamed later.
      CALL CHR2H (12, 'IMLOD       ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, 'TEMP  ', KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = 0
      CATBLK(KIIMU) = NLUSER
      CALL MCREAT (IVOL, LCNO, HBUFF, IERR)
C                                       Blank name back out.
      CALL CHR2H (12, '            ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, '      ', KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = 0
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATING MA FILE FOR IMAGE EXT'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = LCNO
      FRW(NCFILE) = 2
      FVOL(NCFILE) = IVOL
C                                       copy old history
      CALL HISCOP (HLUN, LHLUN, IVOL, FVOL(NCFILE), CNO, LCNO,
     *   CATBLK, HBUFF, LHBUFF, HERR)
      IF (HERR.NE.0) THEN
         WRITE (MSGTXT,1000) HERR, 'ADDING NEW HI TO IMAGE EXT'
         CALL MSGWRT (7)
         END IF
      LINE = 'IMLOD / IMAGE extension header HIstory'
      IF (HERR.EQ.0) CALL HIAD80 (LHLUN, 1, LINE, LHBUFF, HERR)
C                                       parse rest of header.
      ICARD = ICARD + 1
      NAMEXT = ' '
C                                       Loop until END card found.
      DO 20 IREC = 1,100000000
C                                       Read next record.
         IF (ICARD.GT.36) THEN
            CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
            ICARD = 1
            END IF
C                                       Parse card, put value in hdr.
         CALL IMPARS (ICARD, HDRBUF, ISHIST, END, IERR)
         IF (END) GO TO 50
         IST = 80 * ICARD - 79
         CHTEMP = HDRBUF(IST:)
         IF (CHTEMP.EQ.'EXTNAME') THEN
            LINE = HDRBUF(IST:)
            IS = INDEX (LINE, '''')
            IF (IS.GT.0) THEN
               IE = INDEX (LINE(IS+1:), '''')
               IF (IE.GT.1) THEN
                  IE = IE + IS - 1
                  NAMEXT = LINE(IS+1:IE)
                  END IF
               END IF
            END IF
C                                       Add to history file.
         IF (IERR.GT.0) THEN
            DOHI = (CHTEMP.EQ.'HISTORY') .OR. (CHTEMP.EQ.'COMMENT')
     *         .OR. (CHTEMP.EQ.' ')
            LINE = HDRBUF(IST:)
            IST = 1
            IF (DOHI) IST = IST + 8
            IF (HERR.EQ.0) CALL HIAD80 (LHLUN, IST, LINE, LHBUFF, HERR)
         ELSE IF (IERR.EQ.-1) THEN
            IS = (ICARD - 1) * 80 + 1
            IE = IS + 79
            CALL PUTCRD (HDRBUF(IS:IE), IVOL, LCNO, IERR)
            IF (IERR.GT.1) THEN
               WRITE (MSGTXT,1000) IERR, 'CALLING PUTCRD'
               CALL MSGWRT (7)
               GO TO 999
               END IF
            END IF
         ICARD = ICARD + 1
 20      CONTINUE
C                                       Read more cards than we expected
      MSGTXT = 'READ MORE THAN 10**8 CARDS WITHOUT FINDING AN END CARD'
      IERR = 1
      GO TO 990
C                                       End card found.
C                                       Make axis increments non zero
C                                       to help out dumb programs.
 50   IN = KINAX
      IS = KRCIC
      IE = IS + CATBLK(KIDIM) - 1
      DO 60 IAX = IS,IE
         IF ((CATR(IAX).EQ.0.0) .AND. (CATBLK(IN).EQ.1)) CATR(IAX) = 1.0
         IN = IN + 1
 60      CONTINUE
C                                       End FITS header section in HI
      LINE = '/END FITS IMAGE extension header "HISTORY" information'
      IF (HERR.EQ.0) CALL HIADD (LHLUN, LINE, LHBUFF, HERR)
      CALL HICLOS (LHLUN, .TRUE., LHBUFF, IERR)
      IF ((IERR.NE.0) .OR. (HERR.NE.0)) THEN
         HERR = MAX (IERR, HERR)
         WRITE (MSGTXT,1000) HERR, 'WRITING IMAGE EXT HI FILE'
         CALL MSGWRT (7)
         END IF
C                                       reopen main file HI
      CALL HIOPEN (HLUN, IVOL, CNO, HBUFF, HERR)
      IF (.NOT.NODATA) THEN
         TAPEBP = BITPIX
         CALL FITDAT (IVOL, LCNO, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING IMAGE EXT DATA'
            GO TO 990
            END IF
         END IF
C                                       Fill in default names if needed.
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      IF (NAME.EQ.' ') THEN
         NAME = NAMEXT
      ELSE IF (CLASS.EQ.' ') THEN
         CLASS = NAMEXT
         END IF
      SEQ = 0
      SEQ4 = 0
      CALL MLDEF (NAME, CLASS, SEQ)
C                                       Renames to the proper name.
      CALL RNAM (LCNO, IVOL, IERR)
      CALL IMQUIT
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RIMAGE ERROR:',I3,' ON ',A)
 1010 FORMAT ('RIMAGE FOUND ILLEGAL PCOUNT, GCOUNT =',2I5)
      END
