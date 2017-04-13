LOCAL INCLUDE 'FXVLB.INC'
C                                       Include FXVLB
C                                       Local include for FXVLB
      HOLLERITH XNAMEI(3), XCLAIN(2)
      CHARACTER LNAME*12, LCLASS*6
      REAL XINDIS, XINSEQ
      INTEGER INDISK, INSEQ
C                                       Input parameters
      COMMON /INPARM/ XNAMEI, XCLAIN, XINSEQ, XINDIS
      COMMON /INVAL/ INSEQ, INDISK
      COMMON /CHVAL/ LNAME, LCLASS
C                                       Global variables
      INTEGER ICNO, ILUN1, ILUN2
      COMMON /GBLVR/ ICNO, ILUN1, ILUN2
C                                       Buffers
      INTEGER NBUFSZ
      PARAMETER (NBUFSZ = 512)
      INTEGER BUFF1(NBUFSZ), BUFF2(NBUFSZ)
      COMMON /WRKBUF/ BUFF1, BUFF2
LOCAL END
      PROGRAM FXVLB
C-----------------------------------------------------------------------
C! Build CQ table to allow VLBA FX delay decorrelation corrections
C# UV Calibration EXT-util VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007
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
C   Build CQ table to allow VLBA FX delay decorrelation corrections
C   Inputs:
C      AIPS adverb      Local var.     Description
C      INNAME           LNAME          Input uv file name
C      INCLASS          LCLASS         Class of input uv file.
C      INSEQ            INSEQ          Seq. no. of input uv file.
C      INDISK           INDISK         Disk no. of input file.
C----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FXVLB.INC'
      CHARACTER LPGM*6
      INTEGER IRET
      INCLUDE 'INCS:DCAT.INC'
      DATA LPGM /'FXVLB '/
C----------------------------------------------------------------------
C                                       Get input parameters and perform
C                                       general initialisation.
      CALL FXVBIN (LPGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Build the CQ table
      CALL CQMAKE (INDISK, ICNO, CATBLK, BUFF1, BUFF2, ILUN1, ILUN2,
     *   IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Update history file
      CALL FXVBHI (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files/exit
990   CALL DIE (IRET, BUFF1)
C
999   STOP
      END
      SUBROUTINE FXVBIN (LPGM, IRET)
C-----------------------------------------------------------------------
C   Read input parameters for FXVLB and perform general initialisation.
C   Inputs:
C      LPGM    C*6  Task name.
C   Outputs:
C      IRET    I    Return code (0 => ok.)
C-----------------------------------------------------------------------
      CHARACTER LPGM*6
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FXVLB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL   WTRUE, WTABLE, WEXIST, WFITS
      CHARACTER LSTAT*4, LTYPE*2
      INTEGER   NPARM, IERR
      DATA WTRUE /.TRUE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Assign general LUNs for
C                                       table I/O
      ILUN1 = 27
      ILUN2 = 28
C                                       Initialise AIPS from disk
      CALL ZDCHIN (WTRUE)
C                                       Compute pointers for catalog
C                                       records.
      CALL VHDRIN
C                                       Initialise /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input adverbs
      NPARM = 7
      CALL GTPARM (LPGM, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = WTRUE
         IRET = 1
C                                       Check if intiator (AIPS)
C                                       not found.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IERR.NE.0) IRET = 1
C                                       Abort if error obtaining
C                                       input parameters.
      IF (IRET.NE.0) GO TO 999
C                                       Convert input parameters.
      CALL H2CHR (12, 1, XNAMEI, LNAME)
      CALL H2CHR (6, 1, XCLAIN, LCLASS)
      INSEQ = XINSEQ
      INDISK = XINDIS
C                                       Find u-v file in catalog dir.
      LSTAT = 'SRCH'
      LTYPE = 'UV'
      ICNO = 1
      CALL CATDIR ('SRCH', INDISK, ICNO, LNAME, LCLASS, INSEQ, LTYPE,
     *   NLUSER, LSTAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, LNAME, LCLASS, INSEQ, INDISK
         IRET = 2
         GO TO 990
         END IF
C                                       Read catalog header; mark
C                                       file status as 'WRITE'
      CALL CATIO ('READ', INDISK, ICNO, CATBLK, 'WRIT', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR, LNAME, LCLASS, INSEQ, INDISK
         IRET = 3
         GO TO 990
         END IF
C                                       Add to /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = INDISK
      FCNO(NCFILE) = ICNO
      FRW(NCFILE) = 1
C                                       Get u-v header information.
      CALL UVPGET (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         IRET = 4
         GO TO 990
         END IF
C                                       Do not overwrite existing
C                                       CQ table
      CALL ISTAB ('CQ', INDISK, ICNO, 1, ILUN1, BUFF1, WTABLE,
     *   WEXIST, WFITS, IERR)
      IF (WEXIST) THEN
         WRITE (MSGTXT,1200)
         IRET = 5
         GO TO 990
         END IF
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C
999   RETURN
C-----------------------------------------------------------------------
1000  FORMAT ('FXVBIN: ERR',I3,' OBTAINING INPUT PARAMETERS')
1020  FORMAT ('FXVBIN: ERR',I3,' FINDING ',A12,'.',A6,'.',I4,'.',I3)
1040  FORMAT ('FXVBIN: ERR',I3,' READING HEADER ',A12,'.',A6,'.',I4,
     *   '.',I3)
1060  FORMAT ('FXVBIN: ERR',I3,' DECODING HEADER')
1200  FORMAT ('** WARNING - CQ table already exists; will not',
     *   ' overwrite')
      END
      SUBROUTINE FXVBHI (IRET)
C----------------------------------------------------------------------
C   Subroutine to update the history file
C   Output:
C      IRET    I    Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FXVLB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      LOGICAL WUPDAT
      CHARACTER LHIREC*72, LTIME*20
      INTEGER IERR, IDATE(3), ITIME(3)
C----------------------------------------------------------------------
      IRET = 0
C
      CALL HIINIT (3)
C                                       Open history table
      CALL HIOPEN (ILUN1, INDISK, ICNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Task name and time
      CALL ZDATE (IDATE)
      CALL ZTIME (ITIME)
      CALL TIMDAT (ITIME, IDATE, LTIME(13:20), LTIME(1:12))
      WRITE (LHIREC,1010) TSKNAM, RLSNAM, LTIME(1:12), LTIME(13:20)
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       UV-file name
      WRITE (LHIREC,1020) TSKNAM, LNAME, LCLASS, INDISK, INSEQ
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Close HI file
      WUPDAT = .TRUE.
      CALL HICLOS (ILUN1, WUPDAT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
      GO TO 999
C                                       Error
980   WRITE (MSGTXT,1980) IERR
C
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1000  FORMAT ('FXVBHI: ERROR',I3,' OPENING HI TABLE')
1010  FORMAT (A6,' RELEASE: ',A8,' START TIME: ',A12,2X,A8)
1020  FORMAT (A6,' INNAME= ',A12,'.',A6,'.',I3,'.',I4)
1980  FORMAT ('FXVBHI: ERROR',I3,' PROCESSING HISTORY FILE')
      END
