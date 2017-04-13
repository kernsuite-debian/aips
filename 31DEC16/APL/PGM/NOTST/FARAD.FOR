LOCAL INCLUDE 'FARAD.INC'
C                                       Local include for FARAD
C                                       Needs parameters from PUVD.INC.
C                                       AIPS inputs:
      INTEGER   SEQ, DISK, SUBARR, CLVER
      REAL      XSEQ, XDISK, XTIME(8), XANTS(50), XSUBAR, XCLVER,
     *   BPARM(10), TSTART, TEND
      HOLLERITH XNAME(3), XCLASS(2), XINFIL(12), XSRCS(4,30), XOPCOD
C                                       Antenna selection:
      INTEGER   ANTS(50), NANTS
      LOGICAL   DOAWNT
C                                       Source selection:
      INTEGER   SRCS(30), NSRCS
      LOGICAL   DOSWNT
C                                       Circular buffer for TEC data
C                                       (see INITEC for description):
      INTEGER   TECHD, TECTL
      REAL      TEC(24*4), TECLMT(24*4)
C                                       Other ionospheric parameters:
      INTEGER   REFDAT
      REAL      ALT, ANNTIM, MAGLAT, MAGLON, SUNSPT, THICK
      DOUBLE PRECISION TECLAT, TECLON
C                                       CL table record:
      INTEGER   CLRECI(13+32*MAXIF), CLCOLS(MAXCLC), CLNUMV(MAXCLC),
     *   ANTCL, IFRCL, SRCCL, SUBCL, TIMCL
      REAL      CLRECR(13+32*MAXIF)
      DOUBLE PRECISION CLRECD((13+32*MAXIF)/2+1)
C                                       Source coordinates:
      DOUBLE PRECISION SRCDEC(30), SRCRA(30)
C                                       Miscellaneous character data:
      CHARACTER CSRCS(30)*16, INFILE*48, OPCODE*4
C                                       Other miscellaneous global data:
      INTEGER   FIXCNT, CNO, TECFND
C                                       LUNs:
      INTEGER   CLLUN, SULUN, TECLUN
C                                       Fundamental constants:
      DOUBLE PRECISION PI
C                                       Magic number in source
C                                       coordinate table (indicates
C                                       missing values):
      DOUBLE PRECISION MAGIC
C                                       AIPS inputs:
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, XINFIL, XSRCS, XTIME,
     *   XANTS, XSUBAR, XCLVER, XOPCOD, BPARM, SEQ, DISK, SUBARR, CLVER,
     *   TSTART, TEND
C                                       Antenna selection:
      COMMON /ANTSEL/ ANTS, NANTS, DOAWNT
C                                       Source selection:
      COMMON /SRCSEL/ SRCS, NSRCS, DOSWNT
C                                       Circular buffer for TEC data:
      COMMON /TECBUF/ TECHD, TECTL, TEC, TECLMT
C                                       Other ionospheric parameters:
      COMMON /IONDAT/ TECLON, TECLAT, REFDAT, ALT, ANNTIM, MAGLAT,
     *   MAGLON, SUNSPT, THICK
C                                       CL table record:
      COMMON /CLRECC/ CLRECI, CLCOLS, CLNUMV, ANTCL, IFRCL, SRCCL,
     *   SUBCL, TIMCL
C                                       Source coordinates:
      COMMON /SRCCRD/ SRCDEC, SRCRA
C                                       Miscellaneous character data:
      COMMON /FRDCHR/ CSRCS, INFILE, OPCODE
C                                       Other miscellaneous global data:
      COMMON /FRDMSC/ FIXCNT, CNO, TECFND
C                                       CL table record:
      EQUIVALENCE (CLRECI, CLRECR, CLRECD)
C                                       LUNs:
      PARAMETER (CLLUN  = 29)
      PARAMETER (SULUN  = 25)
      PARAMETER (TECLUN = 10)
C                                       Fundamental constants:
      PARAMETER (PI = 3.14159 26535 89793)
C                                       Magic number for source
C                                       coordinate table:
      PARAMETER (MAGIC = 19600907.0)
C                                                 End FARAD
LOCAL END
      PROGRAM FARAD
C-----------------------------------------------------------------------
C! Enters ionospheric Faraday rotation into a CL table.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2015
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
C-----------------------------------------------------------------------
C   Task FARAD calculates the ionospheric Faraday rotation and enters it
C   in a CL table.
C
C   Inputs from AIPS:
C
C   INNAME             Input uv file name (name)
C   INCLASS            Input uv file name (class)
C   INSEQ              Input uv file name (sequence number)
C   INDISK             Input uv file disk number
C   INFILE             Ionospheric data file name
C   SOURCES            Source list
C   TIMERANG           Time range to process
C   ANTENNAS           Antennas to correct
C   SUBARRAY           Subarray
C   GAINVER            CL table version to update
C   OPCODE             Operation code
C   BPARM              Ionospheric model parameters
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   BUFFER(512), IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DUVH.INC'
      PARAMETER (PRGM = 'FARAD ')
C-----------------------------------------------------------------------
C                                       Get input parameters and open
C                                       necessary files:
      CALL FARINI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Scan CL file and apply
C                                       corrections to selected records:
      CALL CLSCAN (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Update history file:
      CALL FARHIS
C                                       Close down:
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE CLSCAN (IRET)
C-----------------------------------------------------------------------
C   Scan through records in the selected CL table, modifying those that
C   meet the selection criteria.
C   Output:
C      IRET     I      Return status: 0 => OK
C                         1 => table I/O error
C                         2 => data correction error
C   Input in common:
C      CATBLK   I(*)   Catalogue header of uv data file
C      CLLUN    I      LUN number for CL file
C      CLVER    I      Version number of CL file
C      CNO      I      Catalogue number of data file
C      DISK     I      AIPS disk number
C      SUBARR   I      Requested subarray
C      XTIME    R(8)   Start and stop times
C      ANTS     I(50)  List of selected/deselected antennae
C      NANTS    I      Number of antennae in ANTS
C      DOAWNT   L      .TRUE. if antennae in ANTS are selected
C      SRCS     I(30)  List of selected/deselected sources
C      NSRCS    I      Number of sources in SRCS
C      DOSWNT   L      .TRUE. if sources in SRCS are selected
C   Output in common:
C      FIXCNT   I      Number of records modified
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   BUFFER(1024), ICLRNO, IERR, NUMANT, NUMIF, NUMPOL,
     *   NUMREC, NTERM
      REAL      GMMOD
      DOUBLE PRECISION TIMBEG, TIMEND
      LOGICAL WNTANT, WNTSRC
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Initialize count of modified
C                                       records (for history file):
      FIXCNT = 0
C                                       Set time range (fractional
C                                       days):
      TIMBEG = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (60.0 * 24.0) +
     *   XTIME(4) / (60.0 * 60.0 * 24.0)
      TIMEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (60.0 * 24.0) +
     *   XTIME(8) / (60.0 * 60.0 * 24.0)
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
      TSTART = TIMBEG
      TEND = TIMEND
C                                       Set number of polarizations and
C                                       IFs in CL table:
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       Open CL table, reformatting if
C                                       necessary:
      CALL CLREFM (DISK, CNO, CLVER, CATBLK, CLLUN, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      CALL CALINI ('WRIT', BUFFER, DISK, CNO, CLVER, CATBLK, CLLUN,
     *   ICLRNO, CLCOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *      IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1001) IERR
         GO TO 990
         END IF
C                                       Get number of records in CL
C                                       table:
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1002)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1003) NUMREC
         GO TO 990
         END IF
C                                       Set up pointers:
      TIMCL = CLCOLS(1)
      SRCCL = CLCOLS(3)
      ANTCL = CLCOLS(4)
      SUBCL = CLCOLS(5)
      IFRCL = CLCOLS(7)
C                                       Scan table:
      DO 100 ICLRNO = 1, NUMREC
C                                       Read record ICLRNO:
         CALL TABIO ('READ', 0, ICLRNO, CLRECR, BUFFER, IERR)
         IF (IERR.GT.0) THEN
            IRET = 1
            WRITE (MSGTXT,1004) IERR
            GO TO 999
            END IF
C                                       Process the record if it is not
C                                       flagged:
         IF (IERR.EQ.0) THEN
C                                       Check if record meets selection
C                                       criteria:
            IF (((CLRECI(SUBCL).EQ.SUBARR) .OR. (CLRECI(SUBCL).LE.0))
     *         .AND. (CLRECD(TIMCL).GE.TIMBEG) .AND.
     *         (CLRECD(TIMCL).LE.TIMEND) .AND. WNTANT (CLRECI(ANTCL))
     *         .AND. WNTSRC (CLRECI(SRCCL))) THEN
               CALL FARCOR (IERR)
               IF (IERR.NE.0) THEN
                  IRET = 2
                  WRITE (MSGTXT,1005) IERR
                  GO TO 990
                  END IF
C                                       Write back modified record:
               CALL TABIO ('WRIT', 0, ICLRNO, CLRECR, BUFFER, IERR)
               IF (IERR.NE.0) THEN
                  IRET = 1
                  WRITE (MSGTXT,1006) IERR
                  GO TO 990
                  END IF
               FIXCNT = FIXCNT + 1
               END IF
            END IF
 100     CONTINUE
C                                       Close down any auxilliary files:
      CALL FARCLS
C                                       Close CL table:
      CALL TABIO ('CLOS', 0, NUMREC, CLRECR, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1100) IERR
         GO TO 990
         END IF
C                                       Summarize processing:
      WRITE (MSGTXT,1101) FIXCNT, NUMREC
      CALL MSGWRT (4)
      IRET = 0
      GO TO 999
C                                       Exception handler:
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLSCAN: ERROR',I2,' REFORMATTING CL TABLE')
 1001 FORMAT ('CLSCAN: ERROR',I2,' OPENING CL TABLE')
 1002 FORMAT ('CLSCAN: CL TABLE HEADER IS CORRUPT')
 1003 FORMAT ('        NUMREC = ',I5)
 1004 FORMAT ('CLSCAN: ERROR',I2,' READING FROM CL TABLE')
 1005 FORMAT ('CLSCAN: ERROR',I2,' CALCULATING ROTATION MEASURE')
 1006 FORMAT ('CLSCAN: ERROR',I2,' WRITING TO CL TABLE')
 1100 FORMAT ('CLSCAN: ERROR',I2,' CLOSING CL TABLE')
 1101 FORMAT ('Modified ',I6,' CL records (out of a total of ',I6,')')
      END
      SUBROUTINE F2CLS
C-----------------------------------------------------------------------
C   Close the file containing f0F2 data.
C   This routine is a place holder for future development.
C-----------------------------------------------------------------------
C
 999  RETURN
      END
      SUBROUTINE F2INI (IRET)
C-----------------------------------------------------------------------
C   Initialize TEC buffers from a file of f0F2 values.
C   This routine is a place holder for future development.
C
C   Output:
C     IRET         I          Exit status: 1 => stubbed routine
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 1
      WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FARAD IS NOT YET CAPABLE OF READING F0F2 DATA')
      END
      SUBROUTINE FARCLS
C-----------------------------------------------------------------------
C   Close down any auxilliary files used for generating the ionospheric
C   rotation measure.
C
C   Inputs in common:
C     OPCODE       C*4        Operation code from AIPS inputs
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
C-----------------------------------------------------------------------
      IF (OPCODE.EQ.'F0F2') THEN
         CALL F2CLS
      ELSE IF (OPCODE.EQ.'TEC') THEN
         CALL TECCLS
      END IF
C
 999  RETURN
      END
      SUBROUTINE FARCOR (IRET)
C-----------------------------------------------------------------------
C   Add the ionospheric Faraday rotation measure to the CL record passed
C   through common /CLRECC/.
C
C   Output:
C     IRET        I        Return code: 0 => OK
C                                       1 => error looking up TEC
C                                       2 => error looking up source
C
C   Inputs in common:
C     CLRECI      I(*)     The CL table record to be corrected
C
C     ALT         R        Altitude of F2 layer (meters)
C
C     GSTIAT      D        GST (radians) at IAT 0 on reference date
C     ROTIAT      D        Rotation rate of the earth in IAT (rad/day)
C
C     SRCDEC      D(*)     List of source declinations (radians)
C     SRCRA       D(*)     List of source RAs (radians)
C
C     STNLAT      D(*)     List of antenna latitudes (radians)
C     STNLON      D(*)     List of antenna longitudes (radians)
C     STNRAD      D(*)     Station radius from Earth centre (meters)
C
C   Output in common:
C     CLRECI      I(*)     Modified record
C
C     SRCDEC      D(*)     List of source declinations (radians)
C     SRCRA       D(*)     List of source RAs (radians)
C                            (May have additional entries filled in)
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IERR
      REAL      LMT, H(3), MAG, TECVAL
      DOUBLE PRECISION AZ, AZSIP, DEC, DLAT, DLONG, HA, LAT, LONG, LST,
     *   RADIUS, THETA, ZENITH, ZENSIP, LATSIP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
C-----------------------------------------------------------------------
C                                       Read source data if necessary:
      IF (SRCDEC(CLRECI(SRCCL)).EQ.MAGIC) THEN
         CALL GETSOU (CLRECI(SRCCL), DISK, CNO, CATBLK, SULUN, IERR)
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
         SRCRA(CLRECI(SRCCL)) = RAAPP
         SRCDEC(CLRECI(SRCCL)) = DECAPP
         END IF
C                                       Read positions from tables:
      DEC = SRCDEC(CLRECI(SRCCL))
      LAT = STNLAT(CLRECI(ANTCL))
      LONG = STNLON(CLRECI(ANTCL))
      RADIUS = STNRAD(CLRECI(ANTCL))
C                                       Calculate the LST at the
C                                       telescope:
      LST = GSTIAT + ROTIAT * CLRECD(TIMCL) + LONG
 10   IF (LST.GE.(2.0 * PI)) THEN
         LST = LST - 2.0 * PI
         GO TO 10
         END IF
 20   IF (LST.LT.0.0) THEN
         LST = LST + 2.0 * PI
         GO TO 20
         END IF
C                                       Calculate azimuth and zenith
C                                       angle of source:
      HA = LST - SRCRA(CLRECI(SRCCL))
      ZENITH =  ACOS (SIN(DEC) * SIN(LAT) + COS(DEC) * COS(LAT) *
     *   COS(HA))
      AZ = ACOS ((SIN(DEC) - SIN(LAT) * COS(ZENITH)) /
     *   (COS(LAT) * SIN(ZENITH)))
      IF (SIN (HA).GT.0.0) AZ = 2 * PI - AZ
C                                       Calculate zenith angle at SIP:
      ZENSIP = ASIN ((RADIUS * SIN (ZENITH)) / (RADIUS + 1000.0 * ALT))
C                                       The sum of the internal angles
C                                       of a triangle gives the great
C                                       circle angle between the antenna
C                                       and the SIP:
      THETA = ZENITH - ZENSIP
C                                       The cosine rule for spherical
C                                       triangles gives the latitude
C                                       of the SIP:
      LATSIP = ASIN (SIN (LAT) * COS (THETA)
     *   + COS (LAT) * SIN (THETA) * COS (AZ))
      DLAT = LATSIP - LAT
C                                       The sine rule for spherical
C                                       triangles gives the remaining
C                                       angles of interest.
      AZSIP = ASIN (SIN (AZ) * COS (LAT) / COS (LATSIP))
      DLONG = ASIN (SIN (AZ) * SIN (THETA) / COS (LATSIP))
C                                       If the source is in the south
C                                       the it should also be in the
C                                       south viewed from the SIP
      IF (ABS(AZ) .GT. PI/2.0) THEN
         IF (AZSIP .GT. 0.0) THEN
            AZSIP = PI - AZSIP
         ELSE
            AZSIP = -PI - AZSIP
            END IF
         END IF
C                                       Calculate LMT (in days) at the
C                                       intersection of the line of
C                                       sight with the ionosphere:
      LMT = CLRECD(TIMCL) + (LONG + DLONG) / (2.0 * PI)
C                                       Look up the TEC at the inter-
C                                       section:
      CALL GETTEC (LMT, TECVAL, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Calculate the magnetic field at
C                                       the intersection, projected
C                                       along the line of sight *from*
C                                       the source:
      CALL MAGDIP (REAL (LAT + DLAT), REAL (LONG + DLONG),
     *   REAL (RADIUS + 1000.0 * ALT), H)
      MAG = -1 * (H(1) * COS (ZENSIP) +
     *   H(2) * SIN (AZSIP) * SIN (ZENSIP) +
     *   H(3) * COS (AZSIP) * SIN (ZENSIP))
C                                       Calculate the rotation measure:
      CLRECR(IFRCL) = 2.62E-17 * MAG * TECVAL / COS (ZENSIP)
      IRET = 0
      GO TO 999
C                                       Exception handler:
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FARCOR: ERROR',I2,' READING SOURCE TABLE')
 1020 FORMAT ('FARCOR: ERROR',I2,' LOOKING UP TEC VALUE')
      END
      SUBROUTINE FARHIS
C-----------------------------------------------------------------------
C   Update history file.
C
C   Inputs in common:
C     TSKNAM        C*(*)     Task name
C     RLSNAM        C*(*)     AIPS release
C
C     CLVER         I         Version number of CL table
C     DISK          I         Disk number for uv data file
C
C     OPCODE        C*4       Operation code
C
C     INFILE        C*48      Ionospheric data file
C
C     SUBARR        I         Subarray number
C     ANTS          I(50)     List of selected/deselected antennae
C     NANTS         I         Number of antennae in ANTS (zero if all
C                             selected)
C     DOAWNT        L         .TRUE. if antennae in ANTS selected
C
C     CSRCS         C(30)*8   List of selected/deselected sources
C     NSRCS         I         Number of sources in CSRCS (zero if all
C                             selected)
C     DOSWNT        L         .TRUE. if sources in SRCS selected
C
C     ALT           R         Altitude of ionosphere (km)
C     THICK         R         Slab thickness of F2 layer (m)
C     SUNSPT        R         Mean monthly smoothed sunspot number
C     TECLAT        D         Latitude of reference point for
C                             ionospheric data (radians)
C     TECLON        D         East longitude of reference point (rad)
C-----------------------------------------------------------------------
      INTEGER   BUFFER(256), DATE(3), HISLUN, I, IERR, J, LIMIT, LIMIT2,
     *   TIME(3)
      CHARACTER CDATE*12, CTIME*8, HILINE*72, KEYWRD*10
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      PARAMETER (HISLUN = 27)
C-----------------------------------------------------------------------
      CALL HIINIT (3)
C                                       Open history file:
      CALL HIOPEN (HISLUN, DISK, FCNO(NCFILE), BUFFER, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       Task message:
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (HILINE,1001) TSKNAM, RLSNAM, CDATE, CTIME
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Sources:
      IF (NSRCS.EQ.0) THEN
         WRITE (HILINE,1002) TSKNAM
         CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
      ELSE
C                                       Included or excluded:
         IF (DOSWNT) THEN
            WRITE (HILINE,1003) TSKNAM
         ELSE
            WRITE (HILINE,1004) TSKNAM
            END IF
         CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Write out sources (this method
C                                       avoids potential problems if
C                                       NSRCS is odd):
         KEYWRD = 'SOURCES  ='
         I = 1
 10      IF (NSRCS.GT.1) THEN
            WRITE (HILINE,1010)  TSKNAM, KEYWRD, CSRCS(I), CSRCS(I+1)
            CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            KEYWRD = '         '
            I = I + 2
            NSRCS = NSRCS - 2
            GO TO 10
            END IF
         IF (NSRCS.EQ.1) THEN
            WRITE (HILINE,1011) TSKNAM, KEYWRD, CSRCS(I)
            CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
         END IF
C                                       Antennae
      IF (NANTS.EQ.0) THEN
         WRITE (HILINE,1012) TSKNAM
         CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
      ELSE
C                                       Included or excluded:
         IF (DOSWNT) THEN
            WRITE (HILINE,1013) TSKNAM
         ELSE
            WRITE (HILINE,1014) TSKNAM
            END IF
         CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Write out antennae:
         LIMIT = MIN (12, NANTS)
         WRITE (HILINE,1015) TSKNAM, (ANTS(I), I = 1, LIMIT)
         CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (LIMIT.LT.NANTS) THEN
            DO 20 I = 13,NANTS,12
               LIMIT = I
               LIMIT2 = MIN (I + 11, NANTS)
               WRITE (HILINE,1016) TSKNAM, (ANTS(J), J = LIMIT, LIMIT2)
               IF (IERR.NE.0) GO TO 100
 20            CONTINUE
            END IF
         END IF
C                                       Time range:
      CALL HITIME (TSTART, TEND, HISLUN, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Subarray and CL version
      WRITE (HILINE,1021) TSKNAM, SUBARR, CLVER
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       OPCODE
      WRITE (HILINE,1022) TSKNAM, OPCODE
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Data file:
      WRITE (HILINE,1023) TSKNAM, INFILE
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Ionospheric parameters:
      WRITE (HILINE,1024) TSKNAM
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1025) TSKNAM, 180.0 * TECLAT / PI
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1026) TSKNAM, 180.0 * TECLON / PI
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1027) TSKNAM, ALT
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1028) TSKNAM, THICK / 1000.0
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1029) TSKNAM, SUNSPT
      CALL HIADD (HISLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C
      GO TO 200
C                                       HI file write errors:
 100  WRITE (MSGTXT,1100) IERR
      CALL MSGWRT (8)
C                                       Close HIstory file:
 200  CALL HICLOS (HISLUN, .TRUE., BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) IERR
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FARHIS: ERROR',I3,' OPENING HISTORY FILE')
 1001 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 1002 FORMAT (A6,' SOURCES =  ''''       / All sources selected')
 1003 FORMAT (A6,' / Sources included:')
 1004 FORMAT (A6,' / Sources excluded:')
 1010 FORMAT (A6,' ',A10,' ''',A16,''',''',A16,'''')
 1011 FORMAT (A6,' ',A10,' ''',A16,'''')
 1012 FORMAT (A6,' ANTENNAS = 0         / All antennae selected')
 1013 FORMAT (A6,' / Antennae included:')
 1014 FORMAT (A6,' / Antennae excluded:')
 1015 FORMAT (A6,' ANTENNAS = ',12(I3,' '))
 1016 FORMAT (A6,'            ',12(I3,' '))
 1021 FORMAT (A6,' SUBARRAY = ',I3,' GAINVER = ',I3)
 1022 FORMAT (A6,' OPCODE   = ''',A4,'''')
 1023 FORMAT (A6,' INFILE   = ''',A48,'''')
 1024 FORMAT (A6,' / Ionospheric parameters:')
 1025 FORMAT (A6,' /   Reference latitude         = ',F7.2,' degrees')
 1026 FORMAT (A6,' /   Reference East longitude   = ',F7.2,' degrees')
 1027 FORMAT (A6,' /   Altitude of F2 layer       = ',F7.2,' km')
 1028 FORMAT (A6,' /   Slab thickness of F2 layer = ',F7.2,' km')
 1029 FORMAT (A6,' /   Sunspot number (R1)        = ',F6.1)
 1100 FORMAT ('FARHIS: ERROR',I3,' WRITING TO HISTORY FILE')
 1200 FORMAT ('FARHIS: ERROR',I3,' CLOSING HISTORY FILE')
      END
      SUBROUTINE FARINI (PRGM, IRET)
C-----------------------------------------------------------------------
C   Get input parameters for FARAD.
C
C   Input:
C     PRGM     C*6      Program name
C
C   Output:
C     IRET     I        Exit code: 0 => OK
C                                  1 => error obtaining inputs
C                                  2 => catalogue error
C                                  3 => input file problem
C                                  4 => other error
C
C   Outputs in common:
C     TSKNAM   C*6      Task name
C     RLSNAM   C*7      Release date
C
C     CNO      I        Catalogue number of data file
C     DISK     I        Disk number for AIPS data
C
C     INFILE   C*48     File containing ionospheric data
C
C     OPCODE   C*4      Operation code
C
C     SUBARR   I        Subarray number
C     ANTS     I(50)    List of selected/deselected antennae
C     NANTS    I        Number of entries in ANTS
C     DOAWNT   L        .TRUE. if antennae in ANTS are selected
C
C     CSRCS    C(30)*8  List of selected/deselected sources
C     SRCS     I(30)    Source numbers corresponding to entries in CSRCS
C     NSRCS    I        Number of entries in SRCS
C     DOSWNT   L        .TRUE. if sources in CSRCS are selected
C
C     REFDAT   I        Day number of reference date
C                       (Jan 1, 1970 = day 1)
C     ANNTIM   R        Annual time of observations (months since
C                       preceding Dec 15th)
C     GSTIAT   D        GST (radians) at IAT 0 on reference date
C     ROTIAT   D        Earth rotation rate (radians/IAT day)
C
C     SRCDEC   D(*)     List of source declinations (radians)
C     SRCRA    D(*)     List of source right ascensions (radians)
C                        (Initialized to blank entries)
C
C     STNLAT   D(*)     List of antenna latitudes (radians)
C     STNLON   D(*)     List of antenna East longitudes (radians)
C     STNRAD   D(*)     List of station radii from Earth center (m)
C
C     ALT      R        Altitude of F2 layer (km)
C     SUNSPT   R        Mean monthly sunspot number (R1)
C     TECLAT   D        Reference latitude for ionospheric parameters
C                       (radians)
C     TECLON   D        Reference East longitude for ionospheric
C                       parameters (radians)
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      INTEGER   BUFFER(1024), IDATE(3), I, IERR, NPARM
      CHARACTER CDATE*8, CLASS*6, NAME*12, STAT*4, UTYPE*2
      INTEGER   DAYNUM, IROUND
      LOGICAL   WNTANT
      REAL      ALTF2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      PARAMETER (NPARM = 210)
C-----------------------------------------------------------------------
C                                       Initialize for AIPS etc.:
      CALL ZDCHIN (.TRUE., BUFFER)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get AIPS parameters:
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 1
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS:
      IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Crunch input parameters:
      SEQ = IROUND (XSEQ)
      DISK = IROUND (XDISK)
      SUBARR = IROUND (XSUBAR)
      IF (SUBARR.LE.0) SUBARR = 1
      CLVER = IROUND (XCLVER)
C                                       Convert characters:
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      DO 10 I = 1, 30
         CALL H2CHR (16, 1, XSRCS(1, I), CSRCS(I))
 10      CONTINUE
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
C                                       Default OPCODE:
      IF (OPCODE.EQ.'    ') OPCODE = 'MOD '
C                                       Find data file and read header:
      CNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISK, CNO, NAME, CLASS, SEQ, UTYPE, NLUSER,
     *   STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         IF (IERR.EQ.5) THEN
            WRITE (MSGTXT,1010) NAME, CLASS, SEQ, DISK, NLUSER
         ELSE
            WRITE (MSGTXT,1011) IERR, NAME, CLASS, SEQ, DISK, NLUSER
            END IF
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK, CNO, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1012) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
C                                       Get uv header info:
      CALL UVPGET (IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1013) IERR
         GO TO 990
         END IF
C                                       Check sort order:
      IF (ISORT(1:2).NE.'TB') THEN
         IRET = 3
         WRITE (MSGTXT,1014) ISORT
         GO TO 990
         END IF
C                                       Find reference date:
      CALL H2CHR (8, 1, CATH(KHDOB), CDATE)
      CALL DATEST (CDATE, IDATE)
      IDATE(1) = IDATE(1) - 1900
      REFDAT = DAYNUM (IDATE(3), IDATE(2), IDATE(1))
      ANNTIM = (REFDAT - DAYNUM (15, 12, IDATE(1)-1)) / 30.6
C                                       Process antenna list:
      DOAWNT = .TRUE.
      NANTS = 0
      DO 100 I = 1, 50
         IF (NINT (XANTS(I)).NE.0) THEN
            IF (NINT (XANTS(I)).LT.0) DOAWNT = .FALSE.
            NANTS = NANTS + 1
            ANTS(NANTS) = ABS (NINT (XANTS(I)))
            END IF
 100     CONTINUE
C                                       Get antenna information:
      CALL GETANT (DISK, CNO, SUBARR, CATBLK, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IRET = 3
         WRITE (MSGTXT,1100) IERR
         GO TO 990
         END IF
C                                       Get source numbers:
      CALL FNDSOU (DISK, CNO, CSRCS, BUFFER, NSRCS, DOSWNT, SRCS, IERR)
      IF (IERR.NE.0) THEN
         IRET = 3
         WRITE (MSGTXT,1101) IERR
         GO TO 990
         END IF
C                                       Initialize table of source
C                                       positions (positions will be
C                                       read as needed):
      DO 110 I = 1, 30
         SRCRA(I) = MAGIC
         SRCDEC(I) = MAGIC
 110     CONTINUE
C                                       Default reference position for
C                                       ionospheric data is the first
C                                       allowed antenna:
      I = 1
 200  IF ((.NOT.WNTANT(I)) .AND. (I.LE.NSTNS)) THEN
         I = I + 1
         GO TO 200
         END IF
      IF (I.GT.NSTNS) THEN
         IRET = 4
         WRITE (MSGTXT,1200)
         GO TO 990
         END IF
      TECLAT = STNLAT(I)
      TECLON = STNLON(I)
C                                       Get altitude of F2 layer and
C                                       sunspot numbers:
      SUNSPT = BPARM(2)
      ALT = BPARM(1)
      IF (ALT.LE.0) THEN
         IF (SUNSPT.LT.0) THEN
            IRET = 4
            WRITE (MSGTXT,1201)
            GO TO 990
            END IF
         ALT = ALTF2 (REAL (TECLAT), REAL (TECLON), ANNTIM, SUNSPT)
         END IF
C                                       Initialize TEC buffers:
      CALL INITEC (IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1202) IERR
         GO TO 999
         END IF
      GO TO 999
C                                       Exception handler:
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FARINI: ERROR',I3,' READING INPUT PARAMETERS')
 1010 FORMAT ('CAN NOT FIND ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' USID=',I5)
 1011 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' USID=',I5)
 1012 FORMAT ('FARINI: ERROR',I3,' READING CATALOG HEADER')
 1013 FORMAT ('FARINI: ERROR',I3,' GETTING HEADER POINTERS')
 1014 FORMAT ('INPUT VISIBILITIES MISSORTED, SORTED = ',A2,
     *   ' (SHOULD BE TB)')
 1100 FORMAT ('FARINI: ERROR',I3,' READING ANTENNAS TABLE')
 1101 FORMAT ('FARINI: ERROR',I3,' READING SOURCE TABLE')
 1200 FORMAT ('FARINI: NO ANTENNAS HAVE BEEN SELECTED!')
 1201 FORMAT ('FARINI: SUNSPOT NUMBER CAN NOT BE NEGATIVE')
 1202 FORMAT ('FARINI: ERROR',I2,' INITIALIZING TEC DATA')
      END
      SUBROUTINE GETTEC (LMT, TECVAL, IRET)
C-----------------------------------------------------------------------
C   Return the TEC corresponding to a given local mean time. Linear
C   interpolation is used between values in the TEC buffer. Data in the
C   TEC buffer is assumed to be in time order. Additional data is read
C   into the buffer as needed.
C
C   Input:
C     LMT         R          Local mean time (days)
C
C   Outputs:
C     TECVAL      R          TEC at given LMT (electrons per square
C                            meter)
C     IRET        I          Exit status: 0 => OK
C                                         1 => unknown OPCODE
C                                         2 => end of TEC data
C                                         3 => error reading TEC data
C                                         4 => gap in TEC data
C                                         5 => LMT before start of
C                                              buffer
C
C   Inputs in common:
C     OPCODE      C*4        Operation code
C
C     REFDAT      I          Day number of reference date (Jan 1, 1970
C                            is day 1)
C     ANNTIM      R          Annual time (days since Dec 15th)
C
C     THICK       R          Equivalent thickness of F2 layer (km)
C     SUNSPT      R          Mean monthly sunspot number (R1)
C     TECLAT      D          Geographic latitude
C     TECLON      D          Geographic East longitude
C     MAGLAT      R          Magnetic latitude
C     MAGLON      R          Magnetic longitude
C
C     TECFND      I          FTAB pointer for TEC data file
C
C   Inputs/outputs in common:
C     TEC         R(96)      TEC buffer (see INITEC)
C     TECHD       I          TEC buffer (see INITEC)
C     TECLMT      R(96)      TEC buffer (see INITEC)
C     TECTL       I          TEC buffer (see INITEC)
C-----------------------------------------------------------------------
      REAL      LMT, TECVAL
      INTEGER   IRET
C
      INTEGER   HEAD, IERR, TAIL, TEMP, VHEAD
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check data is in buffer:
      IF (TECLMT(TECTL).GT.LMT) THEN
         IRET = 5
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
C                                       Read in data if necessary:
      IF (TECHD.EQ.1) THEN
         HEAD = 96
      ELSE
         HEAD = TECHD - 1
         END IF
 10   IF (TECLMT(HEAD).LE.LMT) THEN
         IF (OPCODE.EQ.'TEC ') THEN
            CALL RDTEC (IERR)
         ELSE IF (OPCODE.EQ.'F0F2') THEN
            CALL RDF0F2 (IERR)
         ELSE IF (OPCODE.EQ.'MOD ') THEN
            CALL RDMODL (IERR)
         ELSE
            IRET = 1
            WRITE (MSGTXT,1010) OPCODE
            GO TO 990
            END IF
         IF (IERR.LT.0) THEN
            IRET = 2
            WRITE (MSGTXT,1011)
            GO TO 990
         ELSE IF (IERR.GT.0) THEN
            IRET = 3
            WRITE (MSGTXT,1012) IERR
            GO TO 990
            END IF
         IF (TECHD.EQ.1) THEN
            HEAD = 96
         ELSE
            HEAD = TECHD - 1
            END IF
         GO TO 10
         END IF
C                                       Search for nearest LMT in the
C                                       buffer less than or equal to the
C                                       requested LMT using binary
C                                       search (VHEAD is used to cope
C                                       with wrap-around cases):
      IF (TECHD.LE.TECTL) THEN
         VHEAD = TECHD + 96
      ELSE
         VHEAD = TECHD
         END IF
      TAIL = TECTL - 1
 20   IF (VHEAD.NE.TAIL+1) THEN
         TEMP = (VHEAD + TAIL) / 2
         IF (TEMP.GT.96) TEMP = TEMP - 96
         IF (TECLMT(TEMP).LE.LMT) THEN
            TAIL = TEMP
         ELSE
            HEAD = TEMP
            END IF
         IF (HEAD.LE.TAIL) THEN
            VHEAD = HEAD + 96
         ELSE
            VHEAD = HEAD
            END IF
         GO TO 20
         END IF
C                                       Handle bad data (crude):
      IF ((TEC(TAIL).LT.0.0) .OR. (TEC(HEAD).LT.0.0)) THEN
         IRET = 4
         WRITE (MSGTXT,1020)
         GO TO 990
         END IF
C                                       Linear interpolation:
      TECVAL = TEC(TAIL) +
     *   (LMT - TECLMT(TAIL)) * (TEC(HEAD) - TEC(TAIL)) /
     *   (TECLMT(HEAD) - TECLMT(TAIL))
      GO TO 999
C                                       Exception handler:
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETTEC: REQUESTED LMT BEFORE BEGINNING OF DATA BUFFER')
 1010 FORMAT ('GETTEC: UNKNOWN OPCODE ''',A4,'''')
 1011 FORMAT ('GETTEC: TEC DATA ENDS BEFORE END OF OBSERVATIONS')
 1012 FORMAT ('GETTEC: ERROR',I2,' READING TEC DATA')
 1020 FORMAT ('GETTEC: CAN NOT INTERPOLATE OVER GAPS IN DATA')
      END
      SUBROUTINE INITEC (IRET)
C-----------------------------------------------------------------------
C   Initialize the TEC data buffer. Hourly values of the TEC in units of
C   electrons per meter squared are stored in a circular buffer together
C   with their corresponding local mean times in fractional days. The
C   buffer is implemented in the two arrays TEC (data) and TECLMT
C   (times); the pointers TECHD and TECTL denote the head and tail of
C   the buffer.
C
C   Output:
C     IRET       I            Exit status: 0 => OK
C                                          1 => unknown OPCODE
C                                          2 => other problem
C
C   Inputs in common:
C     OPCODE     C*4          Operation code
C
C     INFILE     C*48         Name of data file
C
C     REFDAT     I            Day number of reference date for
C                             observations (Jan 1, 1970 is day 1)
C     ANNTIM     R            Annual time of observations (months since
C                             preceding Dec 15th)
C
C     ALT        R            Altitude of peak of F2 layer
C     SUNSPT     R            Mean monthly sunspot number (R1)
C     TECLAT     D            Latitude of reference point for data (may
C                             be changed by INITEC)
C     TECLON     D            East longitude of reference point (may be
C                             changed by INITEC)
C
C   Outputs in common:
C     TEC        R(96)        TEC buffer (see above)
C     TECLMT     R(96)        TEC buffer (see above)
C     TECHD      I            TEC buffer (see above)
C     TECTL      I            TEC buffer (see above)
C
C     THICK      R            Equivalent thickness of F2 layer (m):
C                             not calculated if OPCODE='TEC '
C     TECLAT     D            Latitude of reference point
C     TECLON     D            East longitude of reference point
C     MAGLAT     R            Magnetic latitude of reference point
C                             (OPCODE='MOD ' only)
C     MAGLON     R            Magnetic longitude of reference point
C                             (OPCODE='MOD ' only)
C
C     TECFND     I            FTAB index for data file
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IERR
      REAL      SLABF2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Calculate equivalent thickness
C                                       of the F2 layer if required:
      IF ((OPCODE.EQ.'F0F2') .OR. (OPCODE.EQ.'MOD ')) THEN
         THICK = SLABF2 (ALT)
         END IF
C                                       Initialize the buffer:
      IF (OPCODE.EQ.'TEC ') THEN
         CALL TECINI (IERR)
      ELSE IF (OPCODE.EQ.'F0F2') THEN
         CALL F2INI (IERR)
      ELSE IF (OPCODE.EQ.'MOD ') THEN
         CALL MODINI (IERR)
      ELSE
         IRET = 1
         WRITE (MSGTXT,1000) OPCODE
         GO TO 990
         END IF
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1001) IERR
         GO TO 990
         END IF
      GO TO 999
C                                       Exception handler
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('INITEC: UNKNOWN OPCODE ''',A4,'''')
 1001 FORMAT ('INITEC: ERROR',I2,' READING TEC DATA')
      END
      SUBROUTINE MAGCRD (GLAT, GLONG, MLAT, MLONG)
C-----------------------------------------------------------------------
C   MAGCRD converts geographic latitude and longitude into magnetic
C   latitude and longitude. Note that geographic longitude increases to
C   the West while magnetic longitude increases to the East.
C   Inputs:   GLAT     R     geographic latitude (radians)
C             GLONG    R     geographic east-longitude (radians)
C   Outputs:  MLAT     R     magnetic latitude (radians)
C             MLONG    R     magnetic east-longitude (radians)
C   Programmer: C. Flatters, Oct. 1987
C-----------------------------------------------------------------------
      REAL   CMLONG, GLAT, GLATMP, GLONG, GLONMP, MLAT, MLONG, PI,
     *   SMLONG
      PARAMETER (PI = 3.141 592 65)
C                                       Geographic coordinates of
C                                       North magnetic pole.
      PARAMETER (GLATMP = 78.63 * PI / 180)
      PARAMETER (GLONMP = 289.85 * PI / 180)
C-----------------------------------------------------------------------
      MLAT = ASIN (SIN(GLAT) * SIN(GLATMP)
     *             + COS(GLAT) * COS(GLATMP) * COS(GLONG - GLONMP))
      CMLONG = (SIN(GLATMP) * SIN(MLAT) - SIN(GLAT))
     *         / (COS(GLATMP) * COS(MLAT))
      SMLONG = SIN(GLONG - GLONMP) * COS(GLAT) / COS(MLAT)
      MLONG = ATAN2 (SMLONG, CMLONG)
C
 999  RETURN
      END
      SUBROUTINE MODINI (IRET)
C-----------------------------------------------------------------------
C   Initialize TEC buffers using the Chiu model.
C
C   Output:
C     IRET          I               Exit status: 0 => OK
C                                     1 => invalid sunspot number
C
C   Inputs in common:
C     ANNTIM        R               Annual time of observations (months
C                                   from Dec 15)
C     SUNSPT        R               Mean monthly sunspot number (R1)
C     TECLAT        D               Geographic latitude of reference
C                                   point for model
C     TECLON        D               Geographic East longitude of
C                                   reference point
C     THICK         R               Slab thickness of F2 layer (m)
C
C   Outputs in common:
C     TEC           R(96)           TEC value buffer (see INITEC)
C     TECLMT        R(96)           TEC buffer (see INITEC)
C     TECHD         I               TEC buffer (see INITEC)
C     TECTL         I               TEC buffer (see INITEC)
C
C     MAGLAT        R               Magnetic latitude of reference
C                                   point for model (radians)
C     MAGLON        R               Magnetic longitude of reference
C                                   point for model (radians)
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I
      REAL      LASTLT, LMT
      REAL      PEAKF2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check sunspot value:
      IF (SUNSPT.LT.0.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Calculate magnetic coordinates
C                                       of reference point:
      CALL MAGCRD (REAL (TECLAT), REAL (TECLON), MAGLAT, MAGLON)
C                                       Put 24 TEC values in the buffer:
      TECHD = 1
      TECTL = 1
      LASTLT = -1.0
      DO 10 I = 1, 24
         TECLMT(TECHD) = LASTLT + 1.0 / 24.0
         LMT = (TECLMT(TECHD) - INT (TECLMT(TECHD))) * (2.0 * PI)
         IF (LMT.LT.0) LMT = LMT + 2.0 * PI
         TEC(TECHD) = THICK *
     *      PEAKF2 (MAGLAT, MAGLON, ANNTIM, LMT, SUNSPT)
         LASTLT = TECLMT(TECHD)
         TECHD = TECHD + 1
 10      CONTINUE
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODINI: NEGATIVE SUNSPOT NUMBER')
      END
      SUBROUTINE RDF0F2 (IRET)
C-----------------------------------------------------------------------
C   Read a record of data from a file of f0F2 values, use these values
C   to calculate hourly values for the ionospheric TEC and place these
C   in the TEC buffers.
C   This routine is a place holder for future development.
C
C   Output:
C     IRET         I          Exit status: 1 => stubbed routine
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 1
      WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FARAD IS NOT YET CAPABLE OF READING F0F2 DATA')
      END
      SUBROUTINE RDMODL (IRET)
C-----------------------------------------------------------------------
C   'Read' 24 model TEC values into the TEC buffer.
C
C   Output:
C     IRET       I       Exit status: 0 => OK
C                                     1 => invalid inputs
C
C   Inputs in common:
C     ANNTIM     R       Annual time of observation (months since Dec
C                        15)
C     SUNSPT     R       Mean monthly sunspot number (R1)
C     THICK      R       Slab thickness of F2 layer (m)
C
C     TECLAT     D       Geographic latitude of reference point
C                        (radians)
C     MAGLAT     R       Magnetic latitude of reference point for model
C                        (radians)
C     MAGLON     R       Magnetic longitude of reference point (radians)
C
C   Outputs in common:
C     TEC        R(96)   TEC buffer (see INITEC)
C     TECHD      I       TEC buffer (see INITEC)
C     TECLMT     R(96)   TEC buffer (see INITEC)
C     TECTL      I       TEC buffer (see INITEC)
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I
      REAL      LASTLT, LMT
      REAL      PEAKF2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check sunspot number:
      IF (SUNSPT.LT.0.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
      IF (TECHD.EQ.1) THEN
         LASTLT = TECLMT(96)
      ELSE
         LASTLT = TECLMT(TECHD-1)
         END IF
C                                       Make room for new values if the
C                                       buffer is full:
      IF (TECTL.EQ.TECHD) THEN
         TECTL = TECTL + 24
         IF (TECTL.GT.96) TECTL = TECTL - 96
         END IF
C                                       Put values in buffer:
      DO 10 I = 1, 24
         TECLMT(TECHD) = LASTLT + 1.0 / 24.0
         LMT = (TECLMT(TECHD) - INT (TECLMT(TECHD))) * (2.0 * PI)
         IF (LMT.LT.0.0) LMT = LMT + 2 * PI
         TEC(TECHD) = THICK *
     *      PEAKF2 (MAGLAT, MAGLON, ANNTIM, LMT, SUNSPT)
         LASTLT = TECLMT(TECHD)
         TECHD = TECHD + 1
         IF (TECHD.GT.96) TECHD = 1
 10      CONTINUE
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RDMODL: NEGATIVE SUNSPOT NUMBER')
      END
      SUBROUTINE RDTEC (IRET)
C-----------------------------------------------------------------------
C   Read a record of TEC data (Boulder format) into the TEC buffers.
C
C   Output:
C     IRET        I         Exit status: 0 => OK
C                                        1 => Read error
C                                        2 => Reference point changed
C                                        -1 => End of file
C
C   Inputs in common:
C     REFDAT      I         Day number of reference date for
C                           observations (Jan 1, 1970 = day 1)
C
C     TECFND      I         FTAB pointer for TEC data file
C
C     TECLAT      D         Latitude of SIP for TEC data (radians)
C     TECLON      D         East longitude of SIP (radians)
C
C   Inputs/outputs in common:
C     TEC         R(96)     TEC buffer (see INITEC)
C     TECLMT      R(96)     TEC buffer (see INITEC)
C     TECHD       I         TEC buffer (see INITEC)
C     TECTL       I         TEC buffer (see INITEC)
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   CURDAY, DAY, I, IERR, INDEX, MONTH, YEAR
      REAL      TECTMP, UT
      DOUBLE PRECISION LAT, LONG
      CHARACTER BUFF*80
      INTEGER   DAYNUM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Skip non-data records
 10   CALL ZTXIO ('READ', TECLUN, TECFND, BUFF, IERR)
         IF (IERR.EQ.2) THEN
            IRET = -1
            WRITE (MSGTXT,1010)
            GO TO 990
         ELSE IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1011) IERR
            GO TO 990
            END IF
         IF (((BUFF(1:2).NE.'11') .AND. (BUFF(1:2).NE.'12')) .OR.
     *      (BUFF(12:13).NE.'70')) GO TO 10
C                                       Check latitude and longitude:
      READ (BUFF(74:76), 1012) LAT
      READ (BUFF(77:79), 1012) LONG
C                                       Normalize the longitude:
      IF (LONG.GT.180.0) LONG = LONG - 360.0
      LAT = PI * LAT / 180.0
      LONG = PI * LONG / 180.0
      IF ((LAT.NE.TECLAT) .OR. (LONG.NE.TECLON)) THEN
         IRET = 2
         WRITE (MSGTXT,1013)
         GO TO 990
         END IF
C                                       If buffer is full then make room
C                                       for new data:
      IF (TECHD.EQ.TECTL) THEN
         TECTL = TECTL + 12
         IF (TECTL.GT.96) TECTL = TECTL - 96
         END IF
C                                       Put the data into the buffer:
      READ (BUFF(10:11), 1014) DAY
      READ (BUFF(8:9), 1014) MONTH
      READ (BUFF(6:7), 1014) YEAR
      CURDAY = DAYNUM (DAY, MONTH, YEAR)
      INDEX = 14
      IF (BUFF(1:2).EQ.'11') THEN
         UT = 0.0
      ELSE
         UT = 0.5
         END IF
      DO 20 I = 1, 12
         TECLMT(TECHD) = REAL (CURDAY - REFDAT) +
     *      UT + TECLON / (2.0 * PI)
         UT = UT + 1.0 / 24.0
         IF (BUFF(INDEX+4:INDEX+4).EQ.'C') THEN
            TECTMP = -1.0
         ELSE
            READ (BUFF(INDEX:INDEX+2), 1020) TECTMP
            IF (BUFF(INDEX+4:INDEX+4).EQ.'M') THEN
               TECTMP = TECTMP + 1000.0
            ELSE IF (BUFF(INDEX+4:INDEX+4).EQ.'N') THEN
               TECTMP = TECTMP + 2000.0
               END IF
            TECTMP = TECTMP * 1.0E15
            END IF
         TEC(TECHD) = TECTMP
         INDEX = INDEX + 5
         TECHD = TECHD + 1
         IF (TECHD.GT.96) TECHD = 1
 20      CONTINUE
C                                       Successful completion:
      IRET = 0
      GO TO 999
C                                       Exception handler:
 990  CALL MSGWRT (8)
      CALL TECCLS
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('RDTEC: TEC DATA ENDS BEFORE END OF OBSERVATIONS')
 1011 FORMAT ('RDTEC: ERROR',I2,' READING TEC DATA')
 1012 FORMAT (D3.0)
 1013 FORMAT ('RDTEC: REFERENCE POSITION FOR TEC DATA CHANGED')
 1014 FORMAT (I2)
 1020 FORMAT (F3.0)
      END
      SUBROUTINE TECCLS
C-----------------------------------------------------------------------
C   Close the text file containing the TEC data.
C
C   Inputs in common:
C      TECLUN       I       LUN of TEC file
C      TECFND       I       FTAB index of file
C-----------------------------------------------------------------------
      INTEGER IERR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL ZTXCLS (TECLUN, TECFND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTXCLS ERROR ',I4,' CLOSING TEC FILE')
      END
      SUBROUTINE TECINI (IRET)
C-----------------------------------------------------------------------
C   Initialize TEC buffers from a file of hourly TEC values in the
C   Boulder format.
C
C   Output:
C     IRET        I          Exit status: 0 => OK
C                                         1 => could not open data file
C                                         2 => error reading from file
C                                         3 => invalid data file
C                                         4 => no relevant data found
C
C   Inputs in common:
C     INFILE      C*(*)      Name of data file
C     REFDAT      I          Day number of reference date for
C                            observations (Jan 1, 1970 = day 1)
C   Output in common:
C     TEC         R(96)      TEC buffer (see INITEC)
C     TECLMT      R(96)      TEC buffer (see INITEC)
C     TECHD       I          TEC buffer (see INITEC)
C     TECTL       I          TEC buffer (see INITEC)
C
C     TECFND      I          FTAB pointer for TEC data file
C
C     TECLAT      D          Latitude of SIP for TEC data (radians)
C     TECLON      D          East longitude of SIP (radians)
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   CURDAY, DAY, I, IERR, INDEX, MONTH, YEAR
      REAL      TECTMP, UT
      CHARACTER BUFF*80
      INTEGER   DAYNUM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Open the data file:
      CALL ZTXOPN ('READ', TECLUN, TECFND, INFILE, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Read header record:
      CALL ZTXIO ('READ', TECLUN, TECFND, BUFF, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1001) IERR
         GO TO 990
         END IF
C                                       Check header:
      IF (BUFF(1:1).NE.'7') THEN
         IRET = 3
         WRITE (MSGTXT,1002) IERR
         GO TO 990
         END IF
      WRITE (MSGTXT,1003) BUFF(3:20)
      CALL MSGWRT (3)
C                                       Skip data records until just
C                                       before reference date:
      CURDAY = -1
 10   IF (CURDAY .LT. REFDAT-1) THEN
         CALL ZTXIO ('READ', TECLUN, TECFND, BUFF, IERR)
         IF (IERR.EQ.2) THEN
            IRET = 4
            WRITE (MSGTXT,1010)
            GO TO 990
            END IF
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1001) IERR
            GO TO 990
            END IF
C                                       Check type of data record:
         READ (BUFF(10:11), 1011) DAY
         IF (((BUFF(1:2).EQ.'11') .OR. (BUFF(1:2).EQ.'12')) .AND.
     *      (DAY.LT.32) .AND. (BUFF(12:13).EQ.'70')) THEN
            READ (BUFF(8:9), 1011) MONTH
            READ (BUFF(6:7), 1011) YEAR
            CURDAY = DAYNUM (DAY, MONTH, YEAR)
            END IF
         GO TO 10
         END IF
C                                       Store reference latitude and
C                                       longitude:
      READ (BUFF(74:76), 1012) TECLAT
      READ (BUFF(77:79), 1012) TECLON
C                                       Normalize longitude:
      IF (TECLON.GT.180.0) TECLON = TECLON - 360.0
      TECLAT = PI * TECLAT / 180.0
      TECLON = PI * TECLON / 180.0
C                                       Put data into buffer:
      IF (BUFF(80:80).EQ.'L') THEN
         IRET = 3
         WRITE (MSGTXT,1013)
         GO TO 990
         END IF
      TECHD = 1
      TECTL = 1
      INDEX = 14
      IF (BUFF(1:2).EQ.'11') THEN
         UT = 0.0
      ELSE
         UT = 0.5
         END IF
      DO 20 I = 1, 12
         TECLMT(TECHD) = REAL (CURDAY - REFDAT) +
     *      UT + TECLON / (2.0 * PI)
         UT = UT + 1.0 / 24.0
         IF (BUFF(INDEX+4:INDEX+4).EQ.'C') THEN
            TECTMP = -1.0
         ELSE
            READ (BUFF(INDEX:INDEX+2), 1020) TECTMP
            IF (BUFF(INDEX+4:INDEX+4).EQ.'M') THEN
               TECTMP = TECTMP + 1000.0
            ELSE IF (BUFF(INDEX+4:INDEX+4).EQ.'N') THEN
               TECTMP = TECTMP + 2000.0
               END IF
            TECTMP = TECTMP * 1.0E15
            END IF
         TEC(TECHD) = TECTMP
         INDEX = INDEX + 5
         TECHD = TECHD + 1
 20      CONTINUE
C                                       Successful completion:
      IRET = 0
      GO TO 999
C                                       Exception handler:
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TECINI: ERROR',I2,' OPENING DATA FILE')
 1001 FORMAT ('TECINI: ERROR',I2,' READING DATA FILE')
 1002 FORMAT ('TECINI: TEC DATA FILE NOT IN CORRECT FORMAT')
 1003 FORMAT ('Reading TEC data from station ',A18)
 1010 FORMAT ('TECINI: TEC DATA ENDS BEFORE OBSERVATIONS BEGIN')
 1011 FORMAT (I2)
 1012 FORMAT (D3.0)
 1013 FORMAT ('TECINI: CAN NOT HANDLE DATA LABELLED BY LOCAL TIME')
 1020 FORMAT (F3.0)
      END
      REAL FUNCTION ALTF2 (LAT, LONG, ANNTIM, SUNSPT)
C-----------------------------------------------------------------------
C   Calculate the altitude of the peak of the F2 layer using the Chiu
C   model (Chiu, J. At. Terr. Phys. 37:1563, 1975). Diurnal variations
C   are ignored.
C
C   Inputs:
C     LAT      R       Geographic latitude (radians)
C     LONG     R       Geographic East longitude (radians)
C     ANNTIM   R       Annual time (months since Dec 15th)
C     SUNSPT   R       Mean monthly sunspot number (R1)
C
C   Returned value
C     ALTF2    R       Altitude of peak of F2 layer (km)
C-----------------------------------------------------------------------
      REAL LAT, LONG, ANNTIM, SUNSPT
C
      REAL MLAT, MLONG, PI, RHO, SSLDEC, ZETA
      PARAMETER (PI = 3.141 592 65)
C-----------------------------------------------------------------------
C                                       Find magnetic coordinates:
      CALL MAGCRD (LAT, LONG, MLAT, MLONG)
C                                       Calculate the sine of the solar
C                                       declination angle:
      SSLDEC = 0.39795 * SIN (PI / 6.0 * (ANNTIM - 3.167))
C                                       Calculate the seasonal anomoly
C                                       parameter zeta:
      ZETA = SSLDEC * SIN (MLAT)
C
      RHO = SUNSPT / 100.0
      ALTF2 = 240 + 75 * RHO + 83 * RHO * ZETA * COS (MLAT) +
     *   10 * COS (MLAT) * COS (PI / 3.0 * (ANNTIM - 4.5))
C
 999  RETURN
      END
      INTEGER FUNCTION DAYNUM (DAY, MONTH, YEAR)
C-----------------------------------------------------------------------
C   Return the day number of the given date (day 1 is Jan 1st, 1970)
C   The algorithm for calculating the day number within the given year
C   is taken from "Practical Astronomy with your Calculator, 3rd ed."
C   (P. Duffet-Smith, Cambridge, 1988). This function will fail for
C   years outside the range 1970-2099.
C
C   Inputs:
C     DAY        I           day of month
C     MONTH      I           month number
C     YEAR       I           year - 1900 (70 <= YEAR < 2100)
C
C   Returned value:
C     DAYNUM     I           daynumber of given date
C-----------------------------------------------------------------------
      INTEGER   DAY, MONTH, YEAR
C
      INTEGER   Y, IY
C-----------------------------------------------------------------------
C                                       Calculate day number in year:
      IY = YEAR
      IF (IY.LT.70) IY = IY + 100
      Y = IY + 1900
      IF (MONTH.LE.2) THEN
         IF (MOD (Y, 4).EQ.0) THEN
            DAYNUM = 62 * (MONTH - 1) / 2
         ELSE
            DAYNUM = 63 * (MONTH - 1) / 2
            END IF
      ELSE
         IF (MOD (Y, 4).EQ.0) THEN
            DAYNUM = INT (30.6 * REAL (MONTH + 1)) - 62
         ELSE
            DAYNUM = INT (30.6 * REAL (MONTH + 1)) - 63
            END IF
         END IF
      DAYNUM = DAYNUM + DAY
C                                       Count days in previous years:
      IF (Y.NE.1970) THEN
         DO 10 Y = 1970, 1900 + IY - 1
            IF (MOD (Y, 4).EQ.0) THEN
               DAYNUM = DAYNUM + 366
            ELSE
               DAYNUM = DAYNUM + 365
               END IF
 10         CONTINUE
         END IF
C
 999  RETURN
      END
      REAL FUNCTION GAMMA (A, B, ANNTIM)
C-----------------------------------------------------------------------
C   GAMMA returns the gamma function used in the Chiu ionosphere model.
C    Inputs:
C          A        R       dummy parameter
C          B        R       dummy parameter
C          ANNTIM   R       annual time (months), beginning Dec 15th
C   Programmer: C. Flatters, Oct. 1987
C-----------------------------------------------------------------------
      REAL   A, ANNTIM, B, PI
      PARAMETER (PI = 3.141 592 65)
C-----------------------------------------------------------------------
      GAMMA = 1 + A * (B - COS (PI/3 * ANNTIM) + COS (PI/6 * ANNTIM))
C
 999  RETURN
      END
      REAL FUNCTION PEAKF2 (MLAT, MLONG, ANNTIM, LOCTIM, SUNSPT)
C-----------------------------------------------------------------------
C PEAKF2 returns the peak free electron density of the F2-layer in
C electrons per cubic meter. This is derived from a phenomenological
C model of the ionosphere (Chiu, J. At. Terr. Phys. 37, 1563; 1975).
C Some formulae have been corrected according to the code fragment
C IONDEM published as part of the International Reference Ionosphere
C IRI-79 (Report UAG-82, 1981).
C
C Inputs:  MLAT    R    magnetic latitude (radians)
C          MLONG   R    magnetic east-longitude (radians)
C          ANNTIM  R    annual time (months), beginning Dec 15th
C          LOCTIM  R    local time (radians)
C          SUNSPT  R    monthly smoothed Zurich relative sunspot
C                       number
C   Programmer: C. Flatters, Oct. 1987
C-----------------------------------------------------------------------
      REAL   ANNFN, ANNTIM, BETA, BIGG, DIPFN, DIURNL, EQUATR, FOLD, G,
     *     GAMMA, KAPPA, LATFN, LOCTIM, LONGFN, LPRIME, MAGDIP,
     *     MLAT, MLONG, NONPLR, PI, POLAR, PSI, Q, RHO, SIGMA, SOLAR,
     *     SOLDEC, SUNSPT, TILT, W, X, XLONG, Y, ZETA
      EXTERNAL GAMMA, PSI
      PARAMETER (PI = 3.141 592 65)
      PARAMETER (TILT = -23.5 * PI / 180)
C-----------------------------------------------------------------------
C                                     Calculate magnetic dip angle.
      MAGDIP = ATAN (2 * SIN(MLAT) / COS(MLAT))
C                                     Calculate solar declination angle
      SOLDEC = ASIN (0.39795 * SIN (PI/6 * (ANNTIM - 3.167)))
C                                     Calculate the seasonal anomoly
C                                     parameter zeta.
      ZETA = SIN(SOLDEC) * SIN(MLAT)
C                                     Normalize sunspot number.
      RHO = SUNSPT / 100
C                                     Calculate the layer peak function.
C                                     This consists of a polar function
C                                     folded with a non-polar function.
C                                     The polar function dominates at
C                                     high latitudes while the non-polar
C                                     function dominates for lower
C                                     latitudes.
C                                     First calculate the folding factor
      FOLD = EXP (-1 * ((2.4 + (0.4 + 0.1*RHO) * SIN(MLAT)) ** 6)
     *   * COS(MLAT) ** 6)
C                                     Now the polar function. This is
C                                     omitted from Chiu's paper and has
C                                     been reconstructed from a program
C                                     fragment printed in Report UAG-82
C                                     (IRI-79).
      IF (MLAT .GE. 0.0) THEN
         POLAR = (2 + 1.2 * RHO) * (1 + 0.3 * SIN (PI/12 * ANNTIM))
     *      * EXP(-1.2*(COS(MLAT + TILT * COS(LOCTIM)) - COS(MLAT)))
      ELSE
         XLONG = SIN(PI/12 * ANNTIM) * (0.5 * SIN(MLONG/2)
     *      - 0.5 * SIN(MLONG) - (MLONG/2)**8) - (1 + SIN(PI/12 *
     *      ANNTIM)) * COS(PI/6 * ANNTIM) * SIN(MLONG) /
     *      SQRT(ABS(SIN(MLONG))) * EXP (-4 * SIN(MLONG/2)**2)
         POLAR = (1 + 0.4 * (1 - SIN(PI/12 * ANNTIM)**2)
     *      * EXP(-1 * COS(MLONG/2 - PI/20)**4 * SIN(PI/12 * ANNTIM)))
     *      * (2.5 + 2 * RHO + COS(PI/6 * ANNTIM) * (0.5 + (1.3 + 0.2 *
     *      RHO) * COS(MLONG/2 - PI/20)**4) + (1.3 + 0.5 * RHO) *
     *      COS(LOCTIM - PI * (1 + XLONG)))
         END IF
C                                     The non-polar function is the
C                                     product of a solar cycle function,
C                                     a diurnal function, a latitudinal
C                                     function, an annual function, an
C                                     equatorial anomoly function, a
C                                     longitudinal function and a
C                                     magnetic dip function.
C                                     First the solar cycle function.
      SIGMA = 1 + RHO + 0.204 * RHO**2 + 0.03 * RHO**3
      IF (RHO .LE. 1.1) THEN
         SOLAR = SIGMA
      ELSE
         SOLAR = 2.39 + 1.53 * (SIGMA - 2.39) * SIN(MLAT)**2
         END IF
C                                     The diurnal function.
      DIURNL = (0.9 + 0.32 * ZETA)
     *      * (1 + ZETA * COS(LOCTIM - PI/4)**2)
     *      * EXP(-1.1 * (1 + COS (LOCTIM - 0.873)))
C                                     The latitudinal function.
      LPRIME = EXP(3.0 * COS(MLAT/2 * (SIN(LOCTIM) - 1)))
      Q = 1 - 0.15 * EXP(-1 * SQRT((12 * MLAT + 4*PI/3) ** 2
     *   + (ANNTIM/2 - 3) ** 2))
      LATFN = (1.2 - 0.5 * COS(MLAT)**2)
     *   * (1 + 0.05 * RHO * SIN(MLAT)**3
     *   * COS(PI/6 * ANNTIM)) * LPRIME * Q
C                                     The annual function.
      BETA = 1.3 + 0.278 * RHO ** 2 * COS(0.5 * (MLAT - PI/4)) ** 2
     *   + 0.051 * RHO ** 3
      W = EXP (-BETA * (COS(PSI(MLAT, LOCTIM, SOLDEC)) - COS(MLAT)))
      KAPPA = 1 + 0.085 * (COS(MLAT - PI/6)
     *   * COS(PI/12 * (ANNTIM - 2)) ** 3
     *   + COS (MLAT + PI/4) * COS(PI/12 * (ANNTIM - 8)) ** 2)
      X = 0.7 * (KAPPA + 0.178 * RHO**2
     *   * COS(PI/3 * (ANNTIM - 4.3)) / SOLAR) * W
      Y = 0.2 * (1 - SIN(ABS(MLAT) - PI/6))
     *   * (1 + 0.6 * COS(PI/3 * (ANNTIM - 3.94)))
     *   * COS(PI/6 * (ANNTIM - 1))
     *   + (0.13 - 0.06 * SIN(ABS(ABS(MLAT) - PI/9)))
     *   * COS(PI/3 * (ANNTIM - 4.5))
     *   - (0.15 + 0.3 * SIN(ABS(MLAT)))
     *   * (1 - COS(LOCTIM)) ** 0.25
     *   * COS(PSI(MLAT, 0.0, SOLDEC)) ** 3
      ANNFN = X + Y/SOLAR
C                                       The equatorial function.
      BIGG = (1 + 0.6 * SQRT(RHO) - 0.2 * RHO)
     *   * EXP (0.25 * (1 + COS(LOCTIM - 0.873)))
     *   * COS(MLAT)**8 * COS(ABS(MLAT) - 0.2618)**12
      EQUATR = GAMMA(0.05, 0.5, ANNTIM)
     *   * (1 + BIGG) * (1 - 0.4 * COS(MLAT) ** 10)
     *   * (1 + 0.6 * COS(MLAT)**10 * COS(ANNTIM - PI/4)**2)
C                                       The longitudinal function.
      LONGFN = 1 + 0.1 * COS(MLAT)**3
     *   * COS(2 * (MLONG - 7*PI/18))
      G = 0.15 - (1 + RHO) * SIN(MLAT/2)**2
     *   * EXP (-0.33 * (ANNTIM - 6)**2)
C                                       The dip function.
      DIPFN = GAMMA(0.03, 0.5, ANNTIM)
     *   * (1 + G * EXP (-18 * (ABS(MAGDIP) - 2*PI/9)**2))
C                                       Now everything can be put
C                                       together.
      NONPLR = SOLAR * DIURNL * LATFN * ANNFN * EQUATR * LONGFN
     *   * DIPFN
      PEAKF2 = 0.66E11 * (FOLD * POLAR + (1 - FOLD) * NONPLR)
C
 999  RETURN
      END
      REAL FUNCTION PSI (XI, ETA, SDEC)
C-----------------------------------------------------------------------
C PSI is the seasonal anomoly parameter psi(xi,eta). Psi(lat, pi) is
C the solar zenith angle at noon.
C
C Inputs:   XI       R     dummy variable
C           ETA      R     dummy variable
C           SDEC     R     solar declination angle (radians)
C   Programmer: C. Flatters, Oct. 1987
C-----------------------------------------------------------------------
      REAL   ETA, SDEC, XI
C-----------------------------------------------------------------------
      PSI = XI + SDEC * COS(ETA)
C
 999  RETURN
      END
      REAL FUNCTION SLABF2 (ALTPK)
C-----------------------------------------------------------------------
C   Calculate the slab thickness of the F2 layer by integrating over a
C   Chapman profile.
C
C   Input:
C     ALTPK      R      Altitude of peak of F2 layer (km)
C
C   Returned value:
C     SLABF2     R      Equivalent thickness of F2 layer (m)
C-----------------------------------------------------------------------
      REAL ALTPK

      INTEGER I, PROF, UPLIM
      REAL    ALT, R, STEP
C                                       Upper limit of integration:
      PARAMETER (UPLIM = 1024)
C                                       Integration step (km)
      PARAMETER (STEP = 1.0)
C-----------------------------------------------------------------------
C                                       Perform integration (Simpson's
C                                       rule):
      SLABF2 = 0.0
      DO 10 I = 0, UPLIM
         ALT = I * STEP
         IF (ALT.GT.ALTPK) THEN
            R = (ALT - ALTPK) / (40.0 + 0.2 * ALTPK)
         ELSE
            R = (ALT - ALTPK) / (40.0 + 0.2 * ALT)
            END IF
         IF ((I.EQ.0) .OR. (I.EQ.UPLIM)) THEN
            PROF = EXP (1 - R - EXP (-R))
         ELSE IF (MOD (I, 2).EQ.0) THEN
            PROF = 2.0 * EXP (1 - R - EXP (-R))
         ELSE
            PROF = 4.0 * EXP (1 - R - EXP (-R))
            END IF
         SLABF2 = SLABF2 + PROF
 10      CONTINUE
      SLABF2 = 2.0 / 3.0 * 1000.0 * SLABF2
C
 999  RETURN
      END
      LOGICAL FUNCTION WNTANT (ANT)
C-----------------------------------------------------------------------
C   Determine whether a given antenna is selected.
C
C   Input:
C     ANT         I         Antenna number
C
C   Return value:
C     WNTANT      L         .TRUE. if antenna selected, .FALSE.
C                           otherwise
C
C   Inputs in common:
C     ANTS        I(*)      List of selected/deselected antennae
C     DOAWNT      L         .TRUE. if antennae in ANTS are selected
C     NANTS       I         Number of antennae in ANTS
C
C   If NANTS = 0 then all antennae are selected
C-----------------------------------------------------------------------
      INTEGER   ANT
C
      INTEGER   I
      LOGICAL   INANTS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
C-----------------------------------------------------------------------
      IF (NANTS.EQ.0) THEN
         WNTANT = .TRUE.
      ELSE
C                                       Check antenna against antenna
C                                       list:
         INANTS = .FALSE.
         I = 0
 10      IF ((.NOT.INANTS) .AND. (I.NE.NANTS)) THEN
            I = I + 1
            IF (ANTS(I).EQ.ANT) INANTS = .TRUE.
            GO TO 10
            END IF
         IF (DOAWNT) THEN
            WNTANT = INANTS
         ELSE
            WNTANT = .NOT.INANTS
            END IF
         END IF
C
 999  RETURN
      END
      LOGICAL FUNCTION WNTSRC (SRC)
C-----------------------------------------------------------------------
C   Determine whether a given source is selected.
C
C   Input:
C     SRC         I         Source number
C
C   Return value:
C     WNTSRC      L         .TRUE. if source selected, .FALSE.
C                           otherwise
C
C   Inputs in common:
C     DOSWNT      L         .TRUE. if sources in SRCS are selected
C     NSRCS       I         Number of sources in SRCS
C     SRCS        I(*)      List of selected/deselected sources
C
C   If NSRCS = 0 then all sources are selected
C-----------------------------------------------------------------------
      INTEGER   SRC
C
      INTEGER   I
      LOGICAL   INSRCS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FARAD.INC'
C-----------------------------------------------------------------------
      IF (NSRCS.EQ.0) THEN
         WNTSRC = .TRUE.
      ELSE
C                                       Check source against source
C                                       list:
         INSRCS = .FALSE.
         I = 0
 10      IF ((.NOT.INSRCS) .AND. (I.NE.NSRCS)) THEN
            I = I + 1
            IF (SRCS(I).EQ.SRC) INSRCS = .TRUE.
            GO TO 10
            END IF
         IF (DOAWNT) THEN
            WNTSRC = INSRCS
         ELSE
            WNTSRC = .NOT.INSRCS
            END IF
         END IF
C
 999  RETURN
      END
