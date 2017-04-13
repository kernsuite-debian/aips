LOCAL INCLUDE 'ANTAB.INC'
C                                       Include ANTAB
C                                       Local include for ANTAB;
C                                       requires PUVD.INC
      INTEGER MXFQID
C                                       Max. no. of FQ IDs
      PARAMETER (MXFQID = 25)
C
      HOLLERITH XNAMEI(3), XCLAIN(2), XINFIL(12)
C                                       extra stations to ignore
      INTEGER NIGNOR
      PARAMETER (NIGNOR = 10)
      HOLLERITH HIGNOR(2,NIGNOR)
      CHARACTER CIGNOR(NIGNOR)*8
      COMMON /MJUN2/ CIGNOR
C
      CHARACTER LNAME*12, LCLASS*6, LINFIL*48
      REAL XINDIS, XINSEQ, XSUBA, XTYVER, XGCVER, XBLVER, XPRTLV,
     *   XOFFS, DELTAT
      INTEGER INDISK, INSEQ, ISUBA, ITYVER, IGCVER, IBLVER, IPRTLV
C                                       Input parameters
      COMMON /INPARM/ XNAMEI, XCLAIN, XINSEQ, XINDIS, XINFIL,
     *   XSUBA, XTYVER, XGCVER, XBLVER, HIGNOR, XPRTLV, XOFFS
      COMMON /INVAL/ INSEQ, INDISK, ISUBA, ITYVER, IGCVER, IBLVER,
     *   IPRTLV, DELTAT
      COMMON /CHVAL/ LNAME, LCLASS, LINFIL
C                                       Buffers
      INTEGER BUFF1(512), BUFF2(512)
      COMMON /WRKBUF/ BUFF1, BUFF2
C                                       General global variables
      LOGICAL WBFAC, WGAIN, WTSYS
      INTEGER IFQUV(MXFQID), NFQUV, ILUNF, IFINDF, ICNO, NIFUV,
     *   NPOLUV, IPOLUV, ILUN1, ILUN2, IGCTMP, ITYTMP, IREFDY,
     *   ILOYR, IGCVIN, ITYVIN, IDATE(3)
      COMMON /GLBLVR/ IFQUV, NFQUV, ILUNF, IFINDF, ICNO, NIFUV,
     *   NPOLUV, IPOLUV, ILUN1, ILUN2, IGCTMP, ITYTMP, IREFDY,
     *   ILOYR, IGCVIN, ITYVIN, WBFAC, WGAIN, WTSYS, IDATE
C                                       Index table
      INTEGER MAXNX
      PARAMETER (MAXNX = 524288)
      REAL TIMENX(2, MAXNX)
      INTEGER INXSOU(MAXNX), INXFQ(MAXNX), NXDAT
      COMMON /NXCOM/ TIMENX, INXSOU, INXFQ, NXDAT
C                                       FQ information
      DOUBLE PRECISION DFRQTB(MXFQID,MAXIF,2)
      COMMON /FQCOM/ DFRQTB
C                                       Baseline factors
      REAL BFAC(MAXANT,MAXANT)
      COMMON /BSLCOM/ BFAC
LOCAL END
      PROGRAM ANTAB
C-----------------------------------------------------------------------
C! Read VLBI amplitude calibration file into AIPS.
C# UV Calibration EXT-util VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2005, 2007, 2010-2015
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
C   Task ANTAB read a user-supplied amplitude calibration text file
C   and updates the TY, GC and BL tables accordingly.
C   Inputs:
C      AIPS adverb      Local var.     Description
C      INNAME           LNAME          Input uv file name
C      INCLASS          LCLASS         Class of input uv file.
C      INSEQ            INSEQ          Seq. no. of input uv file.
C      INDISK           INDISK         Disk no. of input file.
C      INFILE           LINFIL         Ext. calibration file name.
C      SUBARRAY         ISUBA          Subarray number.
C      TYVER            ITYVER         TY table to update (Tsys)
C      GCVER            IGCVER         GC table to update (gains)
C      BLVER            IBLVER         BL table to update (basl.)
C      PRTLEV           IPRTLV         Print/debug level.
C----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANTAB.INC'
      CHARACTER LPGM*6
      INTEGER   IRET, NTABGC
      DATA LPGM /'ANTAB '/
C----------------------------------------------------------------------
C                                       Get input parameters and perform
C                                       general initialisation.
      CALL ANTBIN (LPGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read calibration information
C                                       from the aux. text file.
C                                       get size of GC table
      CALL TXTCHK (NTABGC, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read calibration information
C                                       from the aux. text file.
C                                       write to GC and TY tables
      CALL TXTCAL (NTABGC, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Consolidate TY, BL and GC tables
      CALL TBCONS (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Update history file
      CALL ANTBHI (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files/exit
990   CALL DIE (IRET, BUFF1)
C
999   STOP
      END
      SUBROUTINE ANTBIN (LPGM, IRET)
C-----------------------------------------------------------------------
C   Read input parameters for ANTAB and perform general initialisation.
C   Inputs:
C      LPGM    C*6  Task name.
C   Outputs:
C      IRET    I    Return code (0 => ok.)
C-----------------------------------------------------------------------
      CHARACTER LPGM*6
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANTAB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WTRUE, WFALSE, WTABLE, WEXIST, WFITS, WMULTI
      CHARACTER LSTAT*4, LTYPE*2, LDATE*8
      INTEGER NPARM, IERR, I
      DATA WTRUE, WFALSE /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       LUN for reading text file
      ILUNF = 10
C                                       Assign general LUNs for
C                                       table I/O.
      ILUN1 = 27
      ILUN2 = 28
C                                       Initialise AIPS from disk
      CALL ZDCHIN (WTRUE, BUFF1)
C                                       Compute pointers for catalog
C                                       records.
      CALL VHDRIN
C                                       Initialise /CFILES/
      NSCR = 0
      NCFILE = 0
C
      NPARM = 25 + 2*NIGNOR
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
      CALL H2CHR (48, 1, XINFIL, LINFIL)
      DO 1 I = 1,NIGNOR
         CALL H2CHR (8, 1, HIGNOR(1,I), CIGNOR(I))
 1       CONTINUE
      INSEQ = XINSEQ
      INDISK = XINDIS
      ISUBA = XSUBA
      ISUBA = MAX (ISUBA, 1)
      ITYVER = XTYVER
      ITYVER = MAX (ITYVER, 0)
      ITYVIN = ITYVER
      IGCVER = XGCVER
      IGCVER = MAX (IGCVER, 0)
      IGCVIN = IGCVER
      IBLVER = XBLVER
      IBLVER = MAX (IBLVER, 0)
      IPRTLV = XPRTLV
C                                       increasing scans in minutes
      DELTAT = XOFFS/(60*24)
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
C                                       cannot do multiple subarray data
      CALL FNDEXT ('AN', CATBLK, I)
      IF (I.GT.1) THEN
         MSGTXT = 'I DO NOT DO MULTIPLE SUBARRAY DATA WELL'
         CALL MSGWRT (6)
         MSGTXT = 'RUN ANTAB ONCE FOR EACH SUBARRAY IN THE DATA SET'
         CALL MSGWRT (6)
         MSGTXT = 'IT IS BETTER TO USE ANTAB BEFORE USUBA OR DBCON'
         CALL MSGWRT (6)
         END IF
C                                       Multi-source or single source ?
      CALL MULSDB (CATBLK, WMULTI)
      IF (WMULTI) THEN
         CALL ISTAB ('SU', INDISK, ICNO, 1, ILUN1, BUFF1, WTABLE,
     *      WEXIST, WFITS, IERR)
         WMULTI = WTABLE .AND. WEXIST .AND. (IERR.EQ.0)
         END IF
      IF (.NOT.WMULTI) THEN
         WRITE (MSGTXT,1080)
         IRET = 5
         GO TO 990
         END IF
C                                       Read NX table into memory
      CALL NXREAD (IERR)
      IF (IERR.NE.0) THEN
         IRET = 6
         GO TO 999
         END IF
C                                       Read FQ table information
      CALL FQREAD (IRET)
      IF (IERR.NE.0) THEN
         IRET = 7
         GO TO 999
         END IF
C                                       Read AN table information
      CALL GETANT (INDISK, ICNO, ISUBA, CATBLK, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         GO TO 999
         END IF
C                                       Open calibration text file.
      CALL ZTXOPN ('READ', ILUNF, IFINDF, LINFIL, WFALSE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR, LINFIL
         IRET = 9
         GO TO 990
         END IF
C                                       Extract reference date and
C                                       calculate reference day no.
      CALL H2CHR (8, 1, CATH(KHDOB), LDATE)
      CALL DATEST (LDATE, IDATE)
      CALL DAYYR (IDATE(1), IDATE(2), IDATE(3), IREFDY, ILOYR)
C                                       Determine FQ-ID/IF/polzn. ranges
C                                       in the uv-data file and so
C                                       determine the ranges in the
C                                       associated tables.
      IF (JLOCIF.GT.0) THEN
         NIFUV = CATBLK(KINAX+JLOCIF)
      ELSE
         NIFUV = 1
         END IF
      IF (JLOCS.GT.0) THEN
         NPOLUV = MIN (2, CATBLK(KINAX+JLOCS))
      ELSE
         NPOLUV = 1
         END IF
      IPOLUV = 1
C                                       First polzn (RCP,Xlin) = 1
      IF ((ICOR0.EQ.-1).OR.(ICOR0.EQ.-5)) IPOLUV = 1
C                                       (LCP,Ylin) = 2
      IF ((ICOR0.EQ.-2).OR.(ICOR0.EQ.-6)) IPOLUV = 2
C                                       For all multi-Stokes datasets
C                                       expect both R and L in the
C                                       tables.
      IF (NPOLUV.GT.1) IPOLUV = 1
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C
999   RETURN
C-----------------------------------------------------------------------
1000  FORMAT ('ANTBIN: ERR',I3,' OBTAINING INPUT PARAMETERS')
1020  FORMAT ('ANTBIN: ERR',I3,' FINDING ',A12,'.',A6,'.',I4,'.',I3)
1040  FORMAT ('ANTBIN: ERR',I3,' READING HEADER ',A12,'.',A6,'.',I4,
     *   '.',I3)
1060  FORMAT ('ANTBIN: ERR',I3,' DECODING HEADER')
1080  FORMAT ('ANTBIN: FILE NOT MULTI-SOURCE OR BAD SU TABLE')
1100  FORMAT ('ANTBIN: ERR',I3,' OPENING ',A48)
      END
      SUBROUTINE NXREAD (IRET)
C-----------------------------------------------------------------------
C   Subroutine to read the NX table into memory
C   Outputs:
C      IRET    I    Return code (0 => ok)
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANTAB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WTABLE, WEXIST, WFITS
      REAL TIME, TINT
      INTEGER NXKOLS(MAXNXC), NXNUMV(MAXNXC), IERR, I, INXRNO, IDSOUR,
     *   ISUBNX, ISTART, IEND, IFQID, NROW, NVISMX, J
C-----------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
      NXDAT = 0
C                                       Does an NX table exist ?
      CALL ISTAB ('NX', INDISK, ICNO, 1, ILUN1, BUFF1, WTABLE, WEXIST,
     *   WFITS, IERR)
      IF ((IERR.NE.0).OR.(.NOT.(WEXIST.AND.WTABLE))) THEN
         IRET = 1
         WRITE (MSGTXT,1020)
         GO TO 990
         END IF
C                                       Open the NX table
      CALL NDXINI ('READ', BUFF1, INDISK, ICNO, 1, CATBLK, ILUN1,
     *   INXRNO, NXKOLS, NXNUMV, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Read table into memory
      NROW = BUFF1(5)
      NVISMX = 0
      DO 100 I = 1, NROW
         CALL TABNDX ('READ', BUFF1, INXRNO, NXKOLS, NXNUMV, TIME,
     *      TINT, IDSOUR, ISUBNX, ISTART, IEND, IFQID, IERR)
C                                       Record de-selected ?
         IF (IERR.EQ.-1) GO TO 100
         IF (IERR.NE.0) THEN
            IRET = 3
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
C                                       Update maximum vis. no.
         NVISMX = MAX (NVISMX, IEND)
C                                       Correct subarray ?
         IF (ISUBNX.NE.ISUBA) GO TO 100
C                                       NX buffer too small ?
         NXDAT = NXDAT + 1
         IF (NXDAT.GT.MAXNX) THEN
            IRET = 4
            WRITE (MSGTXT,1120)
            GO TO 990
            END IF
C
         TIMENX(1,NXDAT) = TIME - TINT / 2.0
         TIMENX(2,NXDAT) = TIME + TINT / 2.0
         INXSOU(NXDAT) = IDSOUR
         INXFQ(NXDAT) = IFQID
100      CONTINUE
C                                       Close NX table
      CALL TABIO ('CLOS', 0, INXRNO, BUFF2, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1140) IERR
         GO TO 990
         END IF
C                                       Determine whether NX table is
C                                       current by matching Nvis in
C                                       catalog hdr. with NVISMX from
C                                       NX table.
      IF (NVISMX.NE.CATBLK(KIGCN)) THEN
         IRET = 5
         WRITE (MSGTXT,1160)
         GO TO 990
         END IF
C                                       Any valid scans found ?
      IF (NXDAT.EQ.0) THEN
         IRET = 6
         WRITE (MSGTXT,1180)
         GO TO 990
         END IF
C                                       Compile table of all unique
C                                       FQ_IDs in the data. Use
C                                       information read from the NX
C                                       table.
      NFQUV = 0
      DO 200 I = 1, NXDAT
         DO 150 J = 1, NFQUV
            IF (IFQUV(J).EQ.INXFQ(I)) GO TO 200
150         CONTINUE
C                                       New FQ-ID found
         NFQUV = NFQUV + 1
         IF (NFQUV.GT.MXFQID) THEN
            WRITE (MSGTXT,1150)
            IRET = 11
            GO TO 990
            END IF
         IFQUV(NFQUV) = INXFQ(I)
200      CONTINUE
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C
999   RETURN
C-----------------------------------------------------------------------
1020  FORMAT ('NXREAD: NO VALID NX TABLE FOUND - RUN INDXR')
1040  FORMAT ('NXREAD: ERR',I3,' OPENING NX TABLE')
1100  FORMAT ('NXREAD: ERR',I3,' READING NX TABLE')
1120  FORMAT ('NXREAD: INCREASE PARAMETER MAXNX')
1140  FORMAT ('NXREAD: ERR',I3,' CLOSING NX TABLE')
1160  FORMAT ('NXREAD: NX TABLE OLD - RUN INDXR')
1180  FORMAT ('NXREAD: NO SCANS FOR SUBARRAY',I4)
1150  FORMAT ('NXREAD: PARAMETER MXFQID NEEDS TO BE INCREASED')
      END
      SUBROUTINE FQREAD (IRET)
C-----------------------------------------------------------------------
C   Subroutine to read parts of the FQ table into memory
C   Outputs:
C      IRET    I    Return code (0 => ok)
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANTAB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DOUBLE PRECISION DFOFF(MAXIF), DBAND, DEDGE
      REAL      FINC(MAXIF)
      INTEGER   ISBAND(MAXIF), NIF, I, J, IVER, IERR, NDIM
      CHARACTER BNDCOD(MAXIF)*8
C-----------------------------------------------------------------------
      IRET = 0
C                                       Initialization
      NDIM = 2 * MXFQID * MAXIF
      CALL DFILL (NDIM, 0.0D0, DFRQTB)
C                                       Read FQ/CH data into memory
      DO 100 I = 1, NFQUV
         IVER = 1
         CALL CHNDAT ('READ', BUFF1, INDISK, ICNO, IVER, CATBLK, ILUN1,
     *      NIF, DFOFF, ISBAND, FINC, BNDCOD, IFQUV(I), IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
C                                       Loop over the IFs
         DO 80 J = 1,NIF
            DBAND = FINC(J) * CATBLK(KINAX+JLOCF)
            DEDGE = DFOFF(J) + CATD(KDCRV+JLOCF)
            IF (ISBAND(J).EQ.-1) DEDGE = DEDGE - DBAND
C                                       Convert to MHz
            DFRQTB(I,J,1) = DEDGE / 1.0D6
            DFRQTB(I,J,2) = (DEDGE + DBAND) / 1.0D6
80          CONTINUE
100      CONTINUE
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C-----------------------------------------------------------------------
1100  FORMAT ('FQREAD: ERR',I3,' READING FQ/CH TABLE')
      END
      SUBROUTINE TXTCHK (NTABGC, IRET)
C----------------------------------------------------------------------
C   Read the auxillary calibration file solely to determine NTABGC
C   Output:
C      NTABGC   I   The maximum number of terms in the gain curve
C      IRET     I   Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER   NTABGC, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PGCV.INC'
      INCLUDE 'ANTAB.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   NPARS, NKEYW, MXPOLY, MEXTRA, MXDUM
      PARAMETER (NKEYW = 34, MXPOLY = 10, MXDUM = 5000000)
      PARAMETER (MEXTRA = MXPOLY + 2*MAXIF + MAXANT + NIGNOR)
      PARAMETER (NPARS = NKEYW + MEXTRA)
C
      LOGICAL   WEOF, WERROR, WTABL, IGMODE, ISVLBA
      CHARACTER LPARS(NPARS)*8, LMARK*8, LINDEX*8, LINDX2*8,
     *   LPOLY*8, LVALS(NPARS)*8, LKEY*8, LDUMMY*8
      DOUBLE PRECISION DUMMY(MXDUM), DVALS(NPARS)
      REAL      POLY(MXPOLY), POLIN(MXPOLY), OFFST, GDEC
      INTEGER   IMAPDF(2,MAXIF), LUNTMP, NCOLDF, I, J, JENTRY, IOFF,
     *   KMODE, ITYP, ISTN, JSTN, NDIM, IGTYPE, IERR, NPOLY, NVALS,
     *   NTAB, NPASSA, NPASSB, IKEY, ANVER, IPLUN
C                                       Recognized KEYIN keywords
      DATA LPARS /'CONTROL ', 'BASELINE', 'GAIN    ', 'TSYS    ',
     *   'TANT    ', 'B       ', 'FT      ', 'TIMEOFF ', 'SRC/SYS ',
     *   'RANGE   ', 'RANGE   ', 'DPFU    ', 'DPFU    ',
     *   'FREQ    ', 'FREQ    ', 'RCP     ', 'LCP     ',
     *   'OFFSET  ', 'EL??    ', 'EQUAT   ', 'ALTAZ   ', 'GCNRAO  ',
     *   'TABLE   ', 'DEC     ',
     *   'A00     ', 'A11O    ', 'A10     ', 'A11E    ', 'A21O    ',
     *   'A20     ', 'A21E    ', 'A22O    ', 'A22E    ', 'A30     ',
     *   MEXTRA * '        '/
      DATA LMARK, LINDEX, LINDX2, LPOLY /'/', 'INDEX','INDEX2', 'POLY'/
C-----------------------------------------------------------------------
C                                       Initialize commons
      NDIM = MAXANT * MAXANT
      CALL RFILL (NDIM, 0.0, BFAC)
      WBFAC = .FALSE.
      WGAIN = .FALSE.
      WTSYS = .FALSE.
C                                       Find out if array is VLBA
C                                       for warning about legacy
C                                       files (see...)
      ANVER = 1
      IPLUN = LUNTMP(1)
      CALL ANTINI ('READ', BUFF1, INDISK, ICNO, ANVER, CATBLK, IPLUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING AN TABLE'
         GO TO 990
         END IF
      CALL TABIO ('CLOS', 1, IANRNO, BUFF1, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
      ISVLBA = ANAME(1:4).EQ.'VLBA'
C                                       initial need unknown
      NTABGC = 0
C
C                                       Set default mapping of Tsys
C                                       column entries to IF/polzn.
C                                       Assume IF and polzn. range
C                                       present in the uv-file are in
C                                       the calibration text file in
C                                       IF-polzn order.
      NDIM = 2 * MAXIF
      CALL FILL (NDIM, 0, IMAPDF)
      JENTRY = 1
      DO 40 I = 1, NIFUV
         DO 20 J = IPOLUV, (IPOLUV+NPOLUV-1)
            IMAPDF(J,I) = JENTRY
            JENTRY = JENTRY + 1
20          CONTINUE
40       CONTINUE
C                                       Default no. of Tsys columns.
      NCOLDF = NIFUV * NPOLUV
C                                       Add keyword POLY to list
      IOFF = NKEYW
      DO 50 I = 1, MXPOLY
         LPARS(I+IOFF) = LPOLY
50       CONTINUE
C                                       Add keywords INDEX and INDEX2
C                                       to list
      IOFF = IOFF + MXPOLY
      DO 60 I = 1, MAXIF
         LPARS(I+IOFF) = LINDEX
         LPARS(I+MAXIF+IOFF) = LINDX2
60       CONTINUE
C                                       Add antenna names as keywords
      IOFF = IOFF + 2 * MAXIF
      DO 70 I = 1, MAXANT
         IF (I.LE.NSTNS) LPARS(I+IOFF) = STNNAM(I)
70       CONTINUE
C                                       IOFF points to end of current LPARS
      IOFF = IOFF + NSTNS
      NPASSA = 0
      DO 72 I = 1,NIGNOR
C                                       examine one entry in CIGNOR at a time
         IF (CIGNOR(I)(1:1).EQ.' ') GO TO 72
         DO 71 J = 1,IOFF
C                                       if entry already appears in LPARS,
C                                       skip it
            IF (CIGNOR(I).EQ.LPARS(J)) GO TO 72
 71         CONTINUE
C                                       CIGNOR entry didnt appear in LPARS
         IOFF = IOFF + 1
C                                       add it.
         LPARS(IOFF) = CIGNOR(I)
         NPASSA = NPASSA + 1
 72      CONTINUE
      IF (NPASSA.NE.0) THEN
         WRITE (MSGTXT, 1072) NPASSA
         CALL MSGWRT (8)
         NPASSB = IOFF - NPASSA
         END IF
C                                       While (NOT EOF and NOT ERROR) do
C                                       Read record;
80    CONTINUE
C                                       Set defaults
      DO 100 J = 1, NPARS
         DVALS(J) = DBLANK
         LVALS(J) = '        '
100      CONTINUE
C                                       Echo KEYIN input if verbose
C                                       print level selected.
      KMODE = 0
      IGMODE = .FALSE.
C
      CALL KEYIN (LPARS, DVALS, LVALS, NPARS, LMARK, KMODE, ILUNF,
     *   IFINDF, IERR)
      WEOF = (IERR.EQ.1)
      WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
      IF (WERROR) THEN
         WRITE (MSGTXT,1000) IERR, 'READING KEYIN FILE'
         GO TO 900
         END IF
C                                       EOF or ERROR encountered
      IF (WEOF.OR.WERROR) GO TO 900
C                                       check if any of the 'ignorable'
C                                       stations were encountered?
      IF (NPASSA.NE.0) THEN
         DO 119 I = NPASSB+1,NPASSB+NPASSA
            IF (DVALS(I).NE.DBLANK) IGMODE = .TRUE.
 119        CONTINUE
         END IF
C                                       Determine record type
      ITYP = 0
      DO 120 I = 1, 5
         IF (DVALS(I).NE.DBLANK) ITYP = I
120      CONTINUE
C                                       Extract first two station
C                                       names in this record
      IOFF = NKEYW + MXPOLY + 2*MAXIF
      ISTN = 0
      JSTN = 0
      DO 130 I = 1, NSTNS
         IF ((DVALS(I+IOFF).NE.DBLANK).AND.(ISTN.EQ.0)) THEN
            ISTN = I
         ELSE IF ((DVALS(I+IOFF).NE.DBLANK).AND.(JSTN.EQ.0)) THEN
            JSTN = I
            END IF
130      CONTINUE
C                                       Case (record_type) of:
C                                       0: Unidentified
      IF (ITYP.EQ.0) THEN
C                                       1: Control group
      ELSE IF (ITYP.EQ.1) THEN
C                                       2: Baseline group
      ELSE IF (ITYP.EQ.2) THEN
C                                       3: Gain record
      ELSE IF (ITYP.EQ.3) THEN
C                                       Extract gain curve type
         IGTYPE = 0
         LKEY = 'EQUAT'
         IF (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) IGTYPE = 1
         LKEY = 'ALTAZ'
         IF (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) IGTYPE = 2
         LKEY = 'EL??'
         IF (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) IGTYPE = 3
         LKEY = 'GCNRAO'
         IF (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) IGTYPE = 4
         WERROR = (IGTYPE.EQ.0)
         IF (WERROR) THEN
            WRITE (MSGTXT,1415)
            GO TO 850
         END IF
C                                       Is the Gain_curve_type
C                                       Tabulated [ or polynomial ? ]
         LKEY = 'TABLE'
         WTABL = (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) .AND.
     *      ((IGTYPE.EQ.1) .OR. (IGTYPE.EQ.2) .OR. (IGTYPE.EQ.3))
C                                       Station must be specified.
         IF (ISTN.EQ.0) THEN
            IF (IGMODE) THEN
C                                       If gain_curve_type is TABLE,
C                                       extra step
               IF (WTABL) GO TO 425
C                                       otherwise skip this record
               GO TO 850
               END IF
            WRITE (MSGTXT,1400)
            WERROR = .TRUE.
            GO TO 850
            END IF
C                                       Initialisation
         GDEC = FBLANK
C                                       Case (Gain_curve_type) of:
C                                       1,2,3: HA, ZA, ELEV
         IF ((IGTYPE.EQ.1) .OR. (IGTYPE.EQ.2) .OR. (IGTYPE.EQ.3)) THEN
C                                       Declination specified ? Only
C                                       allowed for HA gain curves.
            GDEC = FBLANK
            IF (IGTYPE.EQ.1) THEN
               LKEY = 'DEC'
               J = IKEY (LKEY, LPARS, NPARS)
               IF (DVALS(J).NE.DBLANK) GDEC = DVALS(J)
               END IF
C                                       Extract polynomial coeff.
C                                       unless tabulated values expected
            IF (.NOT.WTABL) THEN
               CALL RFILL (MXPOLY, 0.0, POLIN)
               LKEY = 'POLY'
               J = IKEY (LKEY, LPARS, NPARS)
               NPOLY = 0
               DO 420 I = J, (J+MXPOLY-1)
                  IF (DVALS(I).NE.DBLANK) THEN
                     NPOLY = I - J + 1
                     POLIN(NPOLY) = DVALS(I)
                     END IF
 420              CONTINUE
C                                       Offset only allowed for simple
C                                       polynomial.
               OFFST = 0.0
               LKEY = 'OFFSET'
               J = IKEY (LKEY, LPARS, NPARS)
               IF (DVALS(J).NE.DBLANK) OFFST = DVALS(J)
C                                       Adjust poly. coeff. for
C                                       non-zero offset
               IF (OFFST.NE.0.0) THEN
                  CALL POLADJ (POLIN, POLY, NPOLY, OFFST)
               ELSE
                  CALL RCOPY (NPOLY, POLIN, POLY)
                  END IF
               END IF
            END IF
C                                       4: Spherical harmonic coeff.
C                                          for NRAO 140 ft
         IF (IGTYPE.EQ.4) THEN
            NPOLY = 10
            CALL FILL (MXPOLY, 0, POLY)
            LKEY = 'A00'
            J = IKEY(LKEY,LPARS,NPARS)
            IF (DVALS(J).NE.DBLANK) POLY(1) = DVALS(J)
            LKEY = 'A10'
            J = IKEY(LKEY,LPARS,NPARS)
            IF (DVALS(J).NE.DBLANK) POLY(2) = DVALS(J)
            LKEY = 'A11E'
            J = IKEY(LKEY,LPARS,NPARS)
            IF (DVALS(J).NE.DBLANK) POLY(3) = DVALS(J)
            LKEY = 'A11O'
            J = IKEY(LKEY,LPARS,NPARS)
            IF (DVALS(J).NE.DBLANK) POLY(4) = DVALS(J)
            LKEY = 'A20'
            J = IKEY(LKEY,LPARS,NPARS)
            IF (DVALS(J).NE.DBLANK) POLY(5) = DVALS(J)
            LKEY = 'A21E'
            J = IKEY(LKEY,LPARS,NPARS)
            IF (DVALS(J).NE.DBLANK) POLY(6) = DVALS(J)
            LKEY = 'A21O'
            J = IKEY(LKEY,LPARS,NPARS)
            IF (DVALS(J).NE.DBLANK) POLY(7) = DVALS(J)
            LKEY = 'A22E'
            J = IKEY(LKEY,LPARS,NPARS)
            IF (DVALS(J).NE.DBLANK) POLY(8) = DVALS(J)
            LKEY = 'A22O'
            J = IKEY(LKEY,LPARS,NPARS)
            IF (DVALS(J).NE.DBLANK) POLY(9) = DVALS(J)
            LKEY = 'A30'
            J = IKEY(LKEY,LPARS,NPARS)
            IF (DVALS(J).NE.DBLANK) POLY(10) = DVALS(J)
            END IF
C                                       Endcase (gain_curve_type)
C                                       Read tabulated gain values
 425     IF (WTABL) THEN
            KMODE = 3
            NTAB = MXDUM
            CALL KEYIN (LDUMMY, DUMMY, LVALS, NTAB, LMARK, KMODE,
     *         ILUNF, IFINDF, IERR)
            WEOF = (IERR.EQ.1)
            WERROR = ((IERR.NE.0) .AND. (.NOT.WEOF))
            IF (WERROR.OR.WEOF) THEN
               WRITE (MSGTXT,1000) IERR, 'READING TABULATED GAINS'
               GO TO 990
               END IF
            END IF
C                                       you can only get here if IGMODE = T
         IF ((ISTN.EQ.0).AND.IGMODE) GO TO 850
C
         IF (WTABL) THEN
            NTABGC = MAX (NTABGC, NTAB/2)
         ELSE
            NTABGC = MAX (NTABGC, NPOLY)
            END IF
C                                       4: Tsys/Tant group
500   ELSE IF ((ITYP.EQ.4) .OR. (ITYP.EQ.5)) THEN
C
C                                       Read TSYS or TANT values
         KMODE = 3
         NVALS = MXDUM
         CALL KEYIN (LDUMMY, DUMMY, LVALS, NVALS, LMARK, KMODE,
     *      ILUNF, IFINDF, IERR)
         WEOF = (IERR.EQ.1)
         WERROR = ((IERR.NE.0) .AND. (.NOT.WEOF))
         IF (WERROR .OR. WEOF) THEN
            WRITE (MSGTXT,1520) IERR, LPARS(ITYP)
            GO TO 850
            END IF
         END IF
C                                       Endcase (record_type)
850   IF (.NOT.(WERROR.OR.WEOF)) GO TO 80
C                                       Endwhile
900   IF (WERROR) THEN
C                                       General error in cal. proc.
         IRET = 9
         GO TO 990
         END IF
      GO TO 995
C                                       Error
 990  CALL MSGWRT (8)
C                                       Hard close before exit
 995  CALL ZTXCLS (ILUNF, IFINDF, IERR)
      IF (IRET.EQ.0) THEN
         CALL ZTXOPN ('READ', ILUNF, IFINDF, LINFIL, .FALSE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1995) IERR, LINFIL
            IRET = 9
            CALL MSGWRT (8)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TXTCHK: ERROR',I3,' ON ',A)
 1072 FORMAT ('TXTCHK: Added ',I3,' ignorable stations')
 1400 FORMAT ('TXTCHK: GAIN RECORD MISSING STATION NAME')
 1415 FORMAT ('TXTCHK: NO GAIN CURVE TYPE SPECIFIED')
 1520 FORMAT ('TXTCHK: ERROR',I3,' READING ',A4,' VALUES')
 1995 FORMAT ('CHK ERROR',I3,' RE-OPENING ',A)
      END
      SUBROUTINE TXTCAL (NTABGC, IRET)
C----------------------------------------------------------------------
C   Read the auxillary calibration file.
C   Input:
C      NTABGC   I   Number terms in gain curve max
C   Output:
C      IRET     I   Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER   NTABGC, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PGCV.INC'
      INCLUDE 'ANTAB.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   NPARS, NKEYW, MXPOLY, MEXTRA, MXDUM
      PARAMETER (NKEYW = 34, MXPOLY = 10, MXDUM = 5000000)
      PARAMETER (MEXTRA = MXPOLY + 2*MAXIF + MAXANT + NIGNOR)
      PARAMETER (NPARS = NKEYW + MEXTRA)
C
      LOGICAL   WEOF, WERROR, WALLFQ, WTABL, WTSVLA, WRANGE, WSOME,
     *   IGMODE, ISVLBA, PRTWRN
      CHARACTER LPARS(NPARS)*8, LMARK*8, LINDEX*8, LINDX2*8,
     *   LPOLY*8, LVALS(NPARS)*8, LKEY*8, OBSCOD*8, LDUMMY*8
      DOUBLE PRECISION DUMMY(MXDUM), DVALS(NPARS), DSKY1, DSKY2,
     *   REFFRQ
      REAL      POLY(MXPOLY), POLIN(MXPOLY), OFFST, CHNBW, REFPIX, GDEC,
     *   PSS(2), FT, TIMOFF, RTIME, TIMETY, TINTTY,TLOW, THIGH, TMIND,
     *   XVALGC(2,MAXIF), SENSGC(2,MAXIF), TSYST(2,MAXIF),
     *   TANT(2,MAXIF), YVALGC(2,MAXIF,MXTBGC), GAINGC(2,MAXIF,MXTBGC)
      INTEGER   IMAPDF(2,MAXIF), IMAP(2,MAXIF), GCKOLS(MAXGCC),
     *   GCNUMV(MAXGCC), TYKOLS(MAXTYC), TYNUMV(MAXTYC), LUNTMP, NCOLDF,
     *   NCOLKY, I, J, K, M, MM, JENTRY, IOFF, KMODE, ITYP, ISTN, JSTN,
     *   NSKIP, NDIM, IGTYPE, IGPOL, IERR, IGCRNO, NPOLGC,
     *   NOSTKD, STK1, NOBAND, NOCHAN, ITYRNO, NPOLTY, NIFTY, NPOLY,
     *   NVALS, NTAB, NCOLK, NCOLJ, IDAY, IHOUR, ISOUNX, IFQDNX, JPOL,
     *   JIF, IANTTY, ISUBTY, NFOUND, KPOL, NPASSA, NPASSB, IANTGC,
     *   ISUBGC, IFQDGC, ITPGC(2,MAXIF), NTGC(2,MAXIF), IXTGC(2,MAXIF),
     *   IYTGC(2,MAXIF), IKEY, ANVER, IPLUN
C                                       Recognized KEYIN keywords
      DATA LPARS /'CONTROL ', 'BASELINE', 'GAIN    ', 'TSYS    ',
     *   'TANT    ', 'B       ', 'FT      ', 'TIMEOFF ', 'SRC/SYS ',
     *   'RANGE   ', 'RANGE   ', 'DPFU    ', 'DPFU    ',
     *   'FREQ    ', 'FREQ    ', 'RCP     ', 'LCP     ',
     *   'OFFSET  ', 'EL??    ', 'EQUAT   ', 'ALTAZ   ', 'GCNRAO  ',
     *   'TABLE   ', 'DEC     ',
     *   'A00     ', 'A11O    ', 'A10     ', 'A11E    ', 'A21O    ',
     *   'A20     ', 'A21E    ', 'A22O    ', 'A22E    ', 'A30     ',
     *   MEXTRA * '        '/
      DATA LMARK, LINDEX, LINDX2, LPOLY /'/', 'INDEX','INDEX2', 'POLY'/
C-----------------------------------------------------------------------
C                                       Initialize commons
      NDIM = MAXANT * MAXANT
      CALL RFILL (NDIM, 0.0, BFAC)
      WBFAC = .FALSE.
      WGAIN = .FALSE.
      WTSYS = .FALSE.
C                                       Find out if array is VLBA
C                                       for warning about legacy
C                                       files (see...)
      PRTWRN = .FALSE.
      ANVER = 1
      IPLUN = LUNTMP(1)
      CALL ANTINI ('READ', BUFF1, INDISK, ICNO, ANVER, CATBLK, IPLUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      CALL TABIO ('CLOS', 1, IANRNO, BUFF1, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
      ISVLBA = ANAME(1:4).EQ.'VLBA'
C                                       Append results of present
C                                       run to the specified GC table,
C                                       which is consolidated later.
C                                       Open GC table for write
      NPOLGC = NPOLUV
      CALL H2CHR (8, 1, CATH(KHOBS), OBSCOD)
      NOSTKD = CATBLK(KINAX+JLOCS)
      STK1 = ICOR0
      NOBAND = NIFUV
      NOCHAN = CATBLK(KINAX+JLOCF)
      REFFRQ = CATD(KDCRV+JLOCF)
      CHNBW = CATR(KRCIC+JLOCF)
      REFPIX = CATR(KRCRP+JLOCF)
C
      I = NTABGC
      CALL GCINI ('WRIT', BUFF1, INDISK, ICNO, IGCVER, CATBLK, ILUN1,
     *   IGCRNO, GCKOLS, GCNUMV, NPOLGC, NOBAND, I, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
      IF (I.LT.NTABGC) THEN
         IRET = 10
         WRITE (MSGTXT,1065) I, NTABGC
         GO TO 990
         END IF
      NTABGC = I
C                                       Append results of present
C                                       run to the specified TY table,
C                                       which is consolidated later.
C                                       Open TY table for write
      NPOLTY = NPOLUV
      NIFTY = NIFUV
      CALL TYINI ('WRIT', BUFF2, INDISK, ICNO, ITYVER, CATBLK, ILUN2,
     *   ITYRNO, TYKOLS, TYNUMV, NPOLTY, NIFTY, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1070) IERR
         GO TO 990
         END IF
C
C                                       Set default mapping of Tsys
C                                       column entries to IF/polzn.
C                                       Assume IF and polzn. range
C                                       present in the uv-file are in
C                                       the calibration text file in
C                                       IF-polzn order.
      NDIM = 2 * MAXIF
      CALL FILL (NDIM, 0, IMAPDF)
      JENTRY = 1
      DO 40 I = 1, NIFUV
         DO 20 J = IPOLUV, (IPOLUV+NPOLUV-1)
            IMAPDF(J,I) = JENTRY
            JENTRY = JENTRY + 1
20          CONTINUE
40       CONTINUE
C                                       Default no. of Tsys columns.
      NCOLDF = NIFUV * NPOLUV
C                                       Add keyword POLY to list
      IOFF = NKEYW
      DO 50 I = 1, MXPOLY
         LPARS(I+IOFF) = LPOLY
50       CONTINUE
C                                       Add keywords INDEX and INDEX2
C                                       to list
      IOFF = IOFF + MXPOLY
      DO 60 I = 1, MAXIF
         LPARS(I+IOFF) = LINDEX
         LPARS(I+MAXIF+IOFF) = LINDX2
60       CONTINUE
C                                       Add antenna names as keywords
      IOFF = IOFF + 2 * MAXIF
      DO 70 I = 1, MAXANT
         IF (I.LE.NSTNS) LPARS(I+IOFF) = STNNAM(I)
70       CONTINUE
C                                       IOFF points to end of current LPARS
      IOFF = IOFF + NSTNS
      NPASSA = 0
      DO 72 I = 1,NIGNOR
C                                       examine one entry in CIGNOR at a time
         IF (CIGNOR(I)(1:1).EQ.' ') GO TO 72
         DO 71 J = 1,IOFF
C                                       if entry already appears in LPARS,
C                                       skip it
            IF (CIGNOR(I).EQ.LPARS(J)) GO TO 72
 71         CONTINUE
C                                       CIGNOR entry didnt appear in LPARS
         IOFF = IOFF + 1
C                                       add it.
         LPARS(IOFF) = CIGNOR(I)
         NPASSA = NPASSA + 1
 72      CONTINUE
      IF (NPASSA.NE.0) THEN
         WRITE (MSGTXT, 1072) NPASSA
         CALL MSGWRT (8)
         NPASSB = IOFF - NPASSA
         END IF
C
      NSKIP = 0

C                                       While (NOT EOF and NOT ERROR) do
C                                         Read record;
C
80    CONTINUE
C                                       Set defaults
      DO 100 J = 1, NPARS
         DVALS(J) = DBLANK
         LVALS(J) = '        '
100      CONTINUE
C                                       Echo KEYIN input if verbose
C                                       print level selected.
      KMODE = 0
      IF (IPRTLV.GT.0) KMODE = 1
      IGMODE = .FALSE.
C
      CALL KEYIN (LPARS, DVALS, LVALS, NPARS, LMARK, KMODE, ILUNF,
     *   IFINDF, IERR)
      WEOF = (IERR.EQ.1)
      WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
      IF (WERROR) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 900
         END IF
C                                       EOF or ERROR encountered
      IF (WEOF.OR.WERROR) GO TO 900
C                                       check if any of the 'ignorable'
C                                       stations were encountered?
      IF (NPASSA.NE.0) THEN
         DO 119 I = NPASSB+1,NPASSB+NPASSA
            IF (DVALS(I).NE.DBLANK) IGMODE = .TRUE.
 119        CONTINUE
         END IF
C                                       Determine record type
      ITYP = 0
      DO 120 I = 1, 5
         IF (DVALS(I).NE.DBLANK) ITYP = I
120      CONTINUE
C                                       Extract first two station
C                                       names in this record
      IOFF = NKEYW + MXPOLY + 2*MAXIF
      ISTN = 0
      JSTN = 0
      DO 130 I = 1, NSTNS
         IF ((DVALS(I+IOFF).NE.DBLANK).AND.(ISTN.EQ.0)) THEN
            ISTN = I
         ELSE IF ((DVALS(I+IOFF).NE.DBLANK).AND.(JSTN.EQ.0)) THEN
            JSTN = I
            END IF
130      CONTINUE
C                                       Case (record_type) of:
C                                       0: Unidentified
      IF (ITYP.NE.0) GO TO 200
         NSKIP = NSKIP + 1
         GO TO 850
C                                       1: Control group
200   IF (ITYP.NE.1) GO TO 300
C                                       Extract and decode INDEX
C                                       record if specified. This
C                                       then becomes the default
C                                       index for all stations.
         LKEY = 'INDEX'
         J = IKEY (LKEY, LPARS, NPARS)
         LKEY = 'INDEX2'
         K = IKEY (LKEY, LPARS, NPARS)
         IF ((DVALS(J).NE.DBLANK).OR.(DVALS(K).NE.DBLANK)) THEN
            NDIM = 2 * MAXIF
            CALL FILL (NDIM, 0, IMAPDF)
            IF (DVALS(J).NE.DBLANK) CALL DECOD (LVALS(J), MAXIF,
     *         IPOLUV, NPOLUV, NIFUV, IMAPDF, MAXIF, NCOLJ, IERR)
            WERROR = (IERR.NE.0)
            IF (DVALS(K).NE.DBLANK) CALL DECOD (LVALS(K), MAXIF,
     *         IPOLUV, NPOLUV, NIFUV, IMAPDF, MAXIF, NCOLK, IERR)
            NCOLDF = MAX (NCOLJ, NCOLK)
            IF (NCOLDF.GT.MAXIF) THEN
               WERROR = .TRUE.
               WRITE (MSGTXT,1510) MAXIF
               GO TO 850
               END IF
            WERROR = (WERROR.OR.(IERR.NE.0))
            END IF
C
         GO TO 850
C                                       2: Baseline group
300   IF (ITYP.NE.2) GO TO 400
C                                       Are both stations present ?
         IF ((ISTN.EQ.0).OR.(JSTN.EQ.0)) THEN
            IF (IGMODE) GO TO 850
            WRITE (MSGTXT,1150)
            WERROR = .TRUE.
            GO TO 850
            END IF
C                                       Extract baseline factor
         LKEY = 'B'
         J = IKEY (LKEY, LPARS, NPARS)
         IF (DVALS(J).NE.DBLANK) BFAC(TELNO(ISTN),TELNO(JSTN)) =
     *      DVALS(J)
         WBFAC = .TRUE.
C                                       Message about record processed
         WRITE (MSGTXT,1300) STNNAM(ISTN), STNNAM(JSTN)
         CALL MSGWRT (4)
C
         GO TO 850
C                                       3: Gain record
400    IF (ITYP.NE.3) GO TO 425
C                                       First, check gain_curve_type
C
C                                       Extract gain curve type
          IGTYPE = 0
          LKEY = 'EQUAT'
          IF (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) IGTYPE = 1
          LKEY = 'ALTAZ'
          IF (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) IGTYPE = 2
          LKEY = 'EL??'
          IF (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) IGTYPE = 3
          LKEY = 'GCNRAO'
          IF (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) IGTYPE = 4
          WERROR = (IGTYPE.EQ.0)
          IF (WERROR) THEN
             WRITE (MSGTXT,1415)
             GO TO 850
             END IF
C                                       Is the Gain_curve_type
C                                       Tabulated [ or polynomial ? ]
          LKEY = 'TABLE'
          WTABL = (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) .AND.
     *       ((IGTYPE.EQ.1) .OR. (IGTYPE.EQ.2) .OR. (IGTYPE.EQ.3))
C
C
C                                       Station must be specified.
          IF (ISTN.EQ.0) THEN
             IF (IGMODE) THEN
C                                       If gain_curve_type is TABLE, extra step
                IF (WTABL) GO TO 425
C                                       otherwise skip this record
                GO TO 850
                END IF
             WRITE (MSGTXT,1400)
             WERROR = .TRUE.
             GO TO 850
             END IF
C                                       Extract Jy/K
          CALL RFILL (2, 0.0, PSS)
          LKEY = 'DPFU'
          J = IKEY (LKEY, LPARS, NPARS)
          IF ((DVALS(J).EQ.DBLANK).AND.(DVALS(J+1).EQ.DBLANK)) THEN
             WERROR = .TRUE.
             WRITE (MSGTXT,1410)
             GO TO 850
          ELSE
             IF (DVALS(J).NE.DBLANK) PSS(1) = DVALS(J)
             IF (DVALS(J+1).NE.DBLANK) PSS(2) = DVALS(J+1)
             END IF
C                                       Only one DPFU specified
          IF (PSS(1).EQ.0.0) PSS(1) = PSS(2)
          IF (PSS(2).EQ.0.0) PSS(2) = PSS(1)
C
C                                       Initialisation
          GDEC = FBLANK
C                                       Case (Gain_curve_type) of:
C                                       1,2,3: HA, ZA, ELEV
          IF ((IGTYPE.EQ.1).OR.(IGTYPE.EQ.2).OR.(IGTYPE.EQ.3)) THEN
C
C                                       Declination specified ? Only
C                                       allowed for HA gain curves.
             GDEC = FBLANK
             IF (IGTYPE.EQ.1) THEN
                LKEY = 'DEC'
                J = IKEY (LKEY, LPARS, NPARS)
                IF (DVALS(J).NE.DBLANK) GDEC = DVALS(J)
                END IF
C                                       Extract polynomial coeff.
C                                       unless tabulated values expected
             IF (.NOT.WTABL) THEN
                CALL RFILL (MXPOLY, 0.0, POLIN)
                LKEY = 'POLY'
                J = IKEY (LKEY, LPARS, NPARS)
                NPOLY = 0
                DO 420 I = J, (J+MXPOLY-1)
                  IF (DVALS(I).NE.DBLANK) THEN
                     NPOLY = I - J + 1
                     POLIN(NPOLY) = DVALS(I)
                     END IF
420               CONTINUE
C                                       Offset only allowed for simple
C                                       polynomial.
                OFFST = 0.0
                LKEY = 'OFFSET'
                J = IKEY (LKEY, LPARS, NPARS)
                IF (DVALS(J).NE.DBLANK) OFFST = DVALS(J)
C                                       Adjust poly. coeff. for
C                                       non-zero offset
                IF (OFFST.NE.0.0) THEN
                   CALL POLADJ (POLIN, POLY, NPOLY, OFFST)
                ELSE
                   CALL RCOPY (NPOLY, POLIN, POLY)
                   END IF
                END IF
             END IF
C                                       4: Spherical harmonic coeff.
C                                          for NRAO 140 ft.
          IF (IGTYPE.EQ.4) THEN
             CALL FILL (MXPOLY, 0, POLY)
             LKEY = 'A00'
             J = IKEY(LKEY,LPARS,NPARS)
             IF (DVALS(J).NE.DBLANK) POLY(1) = DVALS(J)
             LKEY = 'A10'
             J = IKEY(LKEY,LPARS,NPARS)
             IF (DVALS(J).NE.DBLANK) POLY(2) = DVALS(J)
             LKEY = 'A11E'
             J = IKEY(LKEY,LPARS,NPARS)
             IF (DVALS(J).NE.DBLANK) POLY(3) = DVALS(J)
             LKEY = 'A11O'
             J = IKEY(LKEY,LPARS,NPARS)
             IF (DVALS(J).NE.DBLANK) POLY(4) = DVALS(J)
             LKEY = 'A20'
             J = IKEY(LKEY,LPARS,NPARS)
             IF (DVALS(J).NE.DBLANK) POLY(5) = DVALS(J)
             LKEY = 'A21E'
             J = IKEY(LKEY,LPARS,NPARS)
             IF (DVALS(J).NE.DBLANK) POLY(6) = DVALS(J)
             LKEY = 'A21O'
             J = IKEY(LKEY,LPARS,NPARS)
             IF (DVALS(J).NE.DBLANK) POLY(7) = DVALS(J)
             LKEY = 'A22E'
             J = IKEY(LKEY,LPARS,NPARS)
             IF (DVALS(J).NE.DBLANK) POLY(8) = DVALS(J)
             LKEY = 'A22O'
             J = IKEY(LKEY,LPARS,NPARS)
             IF (DVALS(J).NE.DBLANK) POLY(9) = DVALS(J)
             LKEY = 'A30'
             J = IKEY(LKEY,LPARS,NPARS)
             IF (DVALS(J).NE.DBLANK) POLY(10) = DVALS(J)
             END IF
C                                       Endcase (gain_curve_type)
C
C                                       Extract polzn. type
          IGPOL = 0
          LKEY = 'RCP'
          IF (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) IGPOL = 1
          LKEY = 'LCP'
          IF (DVALS(IKEY(LKEY,LPARS,NPARS)).NE.DBLANK) IGPOL = 2
C                                       Extract sky freq. range for
C                                       this gain information.
          LKEY = 'FREQ'
          J = IKEY (LKEY, LPARS, NPARS)
          DSKY1 = DVALS(J)
          DSKY2 = DVALS(J+1)
C                                       Valid freq. range ?
          WALLFQ = (DSKY1.EQ.DBLANK).OR.(DSKY2.EQ.DBLANK).OR.
     *       (DSKY1.GT.DSKY2)
C
C                                       Gain record (cont''d)
 425      IF (ITYP.NE.3) GO TO 500
C
C                                       Read tabulated gain values
          IF (WTABL) THEN
             KMODE = 3
             IF (IPRTLV.GT.0) KMODE = 4
             NTAB = MXDUM
             CALL KEYIN (LDUMMY, DUMMY, LVALS, NTAB, LMARK, KMODE,
     *          ILUNF, IFINDF, IERR)
             WEOF = (IERR.EQ.1)
             WERROR = ((IERR.NE.0).AND.(.NOT.WEOF))
             IF (WERROR.OR.WEOF) THEN
                WRITE (MSGTXT,1420) IERR
                GO TO 990
                END IF
             END IF
C                                       you can only get here if IGMODE = T
          IF ((ISTN.EQ.0).AND.IGMODE) GO TO 850
C
C                                       Antenna no./ subarray
         IANTGC = TELNO(ISTN)
         ISUBGC = ISUBA
C                                       Loop over FQ-ID/IF/polzn.
         DO 490 I = 1, NFQUV
            IFQDGC = IFQUV(I)
C                                       Fill gain table record
            WSOME = .FALSE.
            NDIM = 2 * MAXIF
            CALL FILL (NDIM, 0, ITPGC)
            CALL FILL (NDIM, 0, NTGC)
            CALL FILL (NDIM, 0, IXTGC)
            CALL FILL (NDIM, 0, IYTGC)
            CALL RFILL (NDIM, FBLANK, XVALGC)
            CALL RFILL (NDIM, FBLANK, SENSGC)
            NDIM = 2 * MAXIF * MXTBGC
            CALL RFILL (NDIM, FBLANK, YVALGC)
            CALL RFILL (NDIM, FBLANK, GAINGC)

            DO 480 J = 1, NIFUV
C                                       Selected by FREQ ?
               IF ((.NOT.WALLFQ).AND.((DFRQTB(I,J,1).LT.DSKY1).OR.
     *            (DFRQTB(I,J,2).GT.DSKY2))) GO TO 480
C                                       Loop over polarization
               DO 470 K = 1, NPOLUV
                  JPOL = K + IPOLUV - 1
C                                       Selected by polarization ?
                  IF ((IGPOL.NE.0).AND.(IGPOL.NE.JPOL)) GO TO 470
                  WSOME = .TRUE.
C                                       Deg. per flux unit
                  IF (JPOL.EQ.1) SENSGC(K,J) = PSS(1)
                  IF (JPOL.EQ.2) SENSGC(K,J) = PSS(2)
C                                       Case (gain_curve_type) of:
C                                       1,2,3: HA, ZA, ELEV
                  IF ((IGTYPE.EQ.1).OR.(IGTYPE.EQ.2).OR.
     *               (IGTYPE.EQ.3)) THEN
C                                       Y-value type
                     IF (IGTYPE.EQ.1) IYTGC(K,J) = 3
                     IF (IGTYPE.EQ.2) IYTGC(K,J) = 2
                     IF (IGTYPE.EQ.3) IYTGC(K,J) = 1
C                                       X-value
                     XVALGC(K,J) = GDEC
C                                       X-type
                     IF (GDEC.EQ.FBLANK) THEN
                        IXTGC(K,J) = 0
                     ELSE
                        IXTGC(K,J) = 4
                        END IF
C                                       Tabulated values or polynomial ?
                     IF (WTABL) THEN
C                                       Tabulated values
                        ITPGC(K,J) = 1
C                                       Must be an even number of values
                        IF (MOD(NTAB,2).NE.0) THEN
                           WERROR = .TRUE.
                           WRITE (MSGTXT,1425)
                           GO TO 850
                           END IF
                        MM = 1
                        DO 440 M = 1, (NTAB-1), 2
                           YVALGC(K,J,MM) = DUMMY(M)
                           GAINGC(K,J,MM) = DUMMY(M+1)
                           MM = MM + 1
440                        CONTINUE
                        NTGC(K,J) = MM - 1
C
                     ELSE IF (NPOLY.NE.0) THEN
C                                       Polynomial
                        ITPGC(K,J) = 2
                        NTGC(K,J) = NPOLY
                        DO 450 M = 1,NPOLY
                           YVALGC(K,J,M) = FBLANK
                           GAINGC(K,J,M) = POLY(M)
450                        CONTINUE
                        END IF
                     END IF
C                                       4: Spherical harmonic coeff (2-D)
                  IF (IGTYPE.EQ.4) THEN
                     ITPGC(K,J) = 3
C                                       Only allow (90-DEC,HA)
                     IXTGC(K,J) = 5
                     IYTGC(K,J) = 3
                     XVALGC(K,J) = FBLANK
                     MM = 0
                     DO 460 M = 1,MXPOLY
                        YVALGC(K,J,M) = FBLANK
                        GAINGC(K,J,M) = POLY(M)
                        IF (POLY(M).NE.0.0) MM = M
460                     CONTINUE
                     NTGC(K,J) = MM
                     END IF
C
470               CONTINUE
480            CONTINUE
C                                       Write GC record
            IF (WSOME) THEN
               CALL TABGC ('WRIT', BUFF1, IGCRNO, GCKOLS, GCNUMV,
     *            NPOLGC, NTABGC, IANTGC, ISUBGC, IFQDGC, ITPGC,
     *            NTGC, IXTGC, IYTGC, XVALGC, YVALGC, GAINGC,
     *            SENSGC, IERR)
               IF (IERR.NE.0) THEN
                  WERROR = .TRUE.
                  WRITE (MSGTXT,1480) IERR
                  GO TO 850
                  END IF
               END IF
            WGAIN = .TRUE.
490         CONTINUE
C                                       Message about record processed
         WRITE (MSGTXT,1490) STNNAM(ISTN)
         CALL MSGWRT (4)
C
         GO TO 850
C                                       4: Tsys/Tant group
500   IF ((ITYP.NE.4).AND.(ITYP.NE.5)) GO TO 850
C                                       Station must be specified
         WERROR = .FALSE.
         IF (ISTN.EQ.0) THEN
            IF (.NOT.IGMODE) THEN
               WRITE (MSGTXT,1500) LPARS(ITYP)
               WERROR = .TRUE.
               GO TO 850
               END IF
            GO TO 525
            END IF
C                                       Should not use *cal.vlba calibration
C                                       files on RDBE data.  Issue warning if:
C                                       (1) not Y; (2) is VLBA; (3) after
C                                       Jan 2014 but before October 2015;
C                                       and (4) the warning has
C                                       not been printed before.
         IF (STNNAM(ISTN)(1:1).NE.'Y'.AND.ISVLBA.AND..NOT.PRTWRN) THEN
           IF ((IDATE(1).GE.14.AND.IDATE(2).GT.1).AND.
     *     (IDATE(1).LT.16.OR.(IDATE(1).EQ.15.AND.IDATE(2).GT.10)))THEN
               PRTWRN = .TRUE.
               WRITE(MSGTXT,1600)
               CALL MSGWRT(8)
               WRITE(MSGTXT,1610)
               CALL MSGWRT(8)
               WRITE(MSGTXT,1620)
               CALL MSGWRT(8)
               WRITE(MSGTXT,1630)
               CALL MSGWRT(8)
               WRITE(MSGTXT,1640)
               CALL MSGWRT(8)
               WRITE(MSGTXT,1650)
               CALL MSGWRT(8)
              ENDIF
            ENDIF
C                                       Extract FT, TIMEOFF, SRC/SYS
C                                       and RANGE
         FT = 1.0
         LKEY = 'FT'
         J = IKEY (LKEY, LPARS, NPARS)
         IF (DVALS(J).NE.DBLANK) FT = DVALS(J)
         TIMOFF = 0.0
         LKEY = 'TIMEOFF'
         J = IKEY (LKEY, LPARS, NPARS)
         IF (DVALS(J).NE.DBLANK) TIMOFF = DVALS(J) / 86400.0D0
         LKEY = 'SRC/SYS'
         J = IKEY (LKEY, LPARS, NPARS)
         WTSVLA = (DVALS(J).NE.DBLANK)
         TLOW = 0.0
         THIGH = FBLANK
         LKEY = 'RANGE'
         J = IKEY (LKEY, LPARS, NPARS)
         IF (DVALS(J).NE.DBLANK) TLOW = DVALS(J)
         IF (DVALS(J+1).NE.DBLANK) THIGH = DVALS(J+1)
C                                       Decode INDEX or INDEX2 keywords
         LKEY = 'INDEX'
         J = IKEY (LKEY, LPARS, NPARS)
         LKEY = 'INDEX2'
         K = IKEY (LKEY, LPARS, NPARS)
         IF ((DVALS(J).NE.DBLANK).OR.(DVALS(K).NE.DBLANK)) THEN
            NDIM = 2 * MAXIF
            CALL FILL (NDIM, 0, IMAP)
            IF (DVALS(K).NE.DBLANK) CALL DECOD (LVALS(K), MAXIF,
     *         IPOLUV, NPOLUV, NIFUV, IMAP, MAXIF, NCOLK, IERR)
            WERROR = (IERR.NE.0)
            IF (DVALS(J).NE.DBLANK) CALL DECOD (LVALS(J), MAXIF,
     *         IPOLUV, NPOLUV, NIFUV, IMAP, MAXIF, NCOLJ, IERR)
            WERROR = (WERROR.OR.(IERR.NE.0))
            NCOLKY = MAX (NCOLK, NCOLJ)
            IF (NCOLKY.GT.MAXIF) THEN
               WERROR = .TRUE.
               WRITE (MSGTXT,1510) MAXIF
               GO TO 850
               END IF
         ELSE
C                                       Else use default mapping
            NDIM = 2 * MAXIF
            CALL COPY (NDIM, IMAPDF, IMAP)
            NCOLKY = NCOLDF
            END IF
C
      IF (WERROR) GO TO 850
C
 525  CONTINUE
C
C                                       Read TSYS or TANT values
      KMODE = 3
      IF (IPRTLV.GT.0) KMODE = 4
      NVALS = MXDUM
      CALL KEYIN (LDUMMY, DUMMY, LVALS, NVALS, LMARK, KMODE,
     *   ILUNF, IFINDF, IERR)
      WEOF = (IERR.EQ.1)
      WERROR = ((IERR.NE.0).AND.(.NOT.WEOF))
      IF (WERROR.OR.WEOF) THEN
         WRITE (MSGTXT,1520) IERR, LPARS(ITYP)
         GO TO 850
         END IF
      IF ((ISTN.EQ.0).AND.IGMODE) GO TO 850
C                                       Fill TY table record
      I = 1
      NFOUND = 0
C                                       While (I <= Nvals) do
530   IF (I.GT.NVALS) GO TO 590
C                                       Compute time relative to
C                                       reference day.
         RTIME = DUMMY(I) - IREFDY
C                                       Experiments going over year-end
         IF (ABS(RTIME+ILOYR).LT.ABS(RTIME)) RTIME = RTIME + ILOYR
         IF (ABS(RTIME-ILOYR).LT.ABS(RTIME)) RTIME = RTIME - ILOYR
         RTIME = RTIME + DUMMY(I+1) / 24.0D0 + TIMOFF
C                                       Check that time is valid
C                                       No experiment assumed to last
C                                       longer than two weeks.
         WERROR = ((RTIME.GT.14).OR.(DUMMY(I).GT.366).OR.
     *      (DUMMY(I).LT.1).OR.(DUMMY(I+1).LT.0.0).OR.
     *      (DUMMY(I+1).GT.24.0))
         IF (WERROR) THEN
C                                       Report bad time
            IDAY = DUMMY(I)
            IHOUR = DUMMY(I+1)
            TMIND = (DUMMY(I+1) - IHOUR) * 60.0
            WRITE (MSGTXT,1530) DUMMY(I), DUMMY(I+1), NFOUND,
     *         STNNAM(ISTN)
            GO TO 850
            END IF
C                                       Skip if this time not in the
C                                       uv-data file.
         CALL NXSRCH (RTIME, ISOUNX, IFQDNX)
         IF (ISOUNX.LE.0) GO TO 560
C                                       Enough values left to read ?
         WERROR = (WERROR.OR.((I+NCOLKY+1).GT.NVALS))
         IF (WERROR) THEN
            WRITE (MSGTXT,1535) LPARS(ITYP)
            GO TO 850
            END IF
C                                       Initialise the Tsys and Tant
C                                       arrays.
         WSOME = .FALSE.
         NDIM = 2 * MAXIF
         CALL RFILL (NDIM, FBLANK, TSYST)
         CALL RFILL (NDIM, FBLANK, TANT)
C                                       Process the Tsys/Tant values
         DO 545 KPOL = 1, NPOLUV
            JPOL = KPOL + IPOLUV - 1
            DO 535 JIF = 1, NIFUV
C                                       Check mapping of col. to pol/IF
               JENTRY = IMAP(JPOL,JIF)
               IF (JENTRY.LE.0) GO TO 535
               K = I + JENTRY + 1
C                                       Check permitted range
               IF (THIGH.NE.FBLANK) THEN
                  WRANGE = ((DUMMY(K).GE.TLOW).AND.(DUMMY(K).LE.THIGH))
               ELSE
                  WRANGE = .TRUE.
                  END IF
               IF ((DUMMY(K).LE.0.0).OR.(ABS(DUMMY(K)-999.9).LT.0.05)
     *            .OR.(.NOT.WRANGE))
     *            THEN
C                                       Invalid temperature
                  IF (ITYP.EQ.4) TSYST(KPOL,JIF) = FBLANK
                  IF (ITYP.EQ.5) TANT(KPOL,JIF) = FBLANK
               ELSE
C                                       Valid temperature
                  WSOME = .TRUE.
                  IF (ITYP.EQ.4) THEN
                     TSYST(KPOL,JIF) = FT * DUMMY(K)
C                                       Mark Ta/Tsys by setting Tant
C                                       to -1.0
                     IF (WTSVLA) TANT(KPOL,JIF) = -1.0
                     END IF
                  IF (ITYP.EQ.5) TANT(KPOL,JIF) = DUMMY(K)
                  END IF
535            CONTINUE
545         CONTINUE
C                                       Write the TY record
         TIMETY = RTIME
         TINTTY = 0.0
         IANTTY = TELNO(ISTN)
         ISUBTY = ISUBA
         IF (WSOME) THEN
            CALL TABTY ('WRIT', BUFF2, ITYRNO, TYKOLS, TYNUMV, NPOLTY,
     *         NIFTY, TIMETY, TINTTY, ISOUNX, IANTTY, ISUBTY, IFQDNX,
     *         TSYST, TANT, IERR)
            IF (IERR.NE.0) THEN
               WERROR = .TRUE.
               WRITE (MSGTXT,1545) IERR
               GO TO 850
               END IF
            END IF
         WTSYS = .TRUE.
         NFOUND = NFOUND + 1
C                                       Increment pointer in array of
C                                       Tsys or Tant values.
560      I = I + NCOLKY + 2
         GO TO 530
C                                       Endwhile
590   CONTINUE
C                                       Message about data loaded
      WRITE (MSGTXT,1590) NFOUND, LPARS(ITYP), STNNAM(ISTN)
      CALL MSGWRT (4)
      GO TO 850
C                                       Endcase (record_type)
850   IF (.NOT.(WERROR.OR.WEOF)) GO TO 80
C                                       Endwhile
900   IF (WERROR) THEN
C                                       General error in cal. proc.
         IRET = 9
         GO TO 990
         END IF
C                                       Message about records skipped
      WRITE (MSGTXT,1900) NSKIP
      CALL MSGWRT (4)
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Hard close before exit
999   CALL TABIO ('CLOS', 0, IGCRNO, BUFF1, BUFF1, IERR)
      CALL TABIO ('CLOS', 0, ITYRNO, BUFF2, BUFF2, IERR)
      CALL ZTXCLS (ILUNF, IFINDF, IERR)
      RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('TXTCAL: ERROR ',I3,' READING AN TABLE')
 1050 FORMAT ('TXTCAL: ERROR',I3,' CREATING GC TABLE')
 1065 FORMAT ('TXTCAL: EXISTING GC TABLE HAS',I4,' TERMS, NEED',I4)
 1070 FORMAT ('TXTCAL: ERROR',I3,' CREATING TY TABLE')
 1072 FORMAT ('TXTCAL: Added ',I3,' ignorable stations')
 1100 FORMAT ('TXTCAL: ERROR',I3,' READING CAL. FILE')
 1150 FORMAT ('TXTCAL: BASELINE RECORD WITHOUT TWO ANTENNAS')
 1300 FORMAT ('Baseline record read for: ',A8,' - ',A8)
 1400 FORMAT ('TXTCAL: GAIN RECORD MISSING STATION NAME')
 1410 FORMAT ('TXTCAL: GAIN RECORD MISSING DPFU (K/JY)')
 1415 FORMAT ('TXTCAL: NO GAIN CURVE TYPE SPECIFIED')
 1420 FORMAT ('TXTCAL: ERROR',I3,' READING TABULATED GAINS')
 1425 FORMAT ('TXTCAL: ODD NUMBER OF TABULATED GAIN VALUES')
 1480 FORMAT ('TXTCAL: ERROR',I3,' WRITING TO GC TABLE')
 1490 FORMAT ('Gain curve read for station: ',A8)
 1500 FORMAT ('TXTCAL: ',A4,' RECORD MISSING STATION NAME')
 1510 FORMAT ('TXTCAL: NO. OF INDEXED COLUMNS EXCEEDS: ',I3)
 1520 FORMAT ('TXTCAL: ERROR',I3,' READING ',A4,' VALUES')
 1530 FORMAT ('TXTCAL: BAD TIME: ',2F8.2,' after',I4,
     *   ' entries: ', A8)
 1535 FORMAT ('TXTCAL: INCOMPLETE ENTRY OF ',A4,' VALUES')
 1545 FORMAT ('TXTCAL: ERROR',I3,' WRITING TY TABLE')
 1590 FORMAT (I6,1X,A4,' records read for station: ',A8)
 1600 FORMAT ('! WARNING: If you are using Tsys infomation from the
     * *cal.vlba')
 1610 FORMAT ('! file for projects observed between the dates of
     * January 1,')
 1620 FORMAT ('! 2014 and October 19, 2015 then please be aware that
     * this data is')
 1630 FORMAT ('! from the VLBA LEGACY system.  The LEGACY system has
     * a different')
 1640 FORMAT ('! signal path from the RDBE/MARK5C system.  Do not
     * use *cal.vlba')
 1650 FORMAT ('! file FROM DATES ABOVE unless you understand what you
     * are doing.')
 1900 FORMAT (I6,' unidentified records skipped on input')
      END
      FUNCTION IKEY (LKEY, LPARS, NPARS)
C-----------------------------------------------------------------------
C   Routine to search array LPARS for string LKEY
C   Inputs:
C      LKEY    C*8     String to search for
C      LPARS   C*8(*)  Array to be searched
C      NPARS   I       Length of LPARS
C   Outputs:
C      IKEY    I       Index in LPARS; 0 if not found.
C-----------------------------------------------------------------------
      INTEGER IKEY, NPARS
      CHARACTER*8 LKEY, LPARS(NPARS)
C
      INCLUDE 'INCS:DMSG.INC'
      INTEGER I
C-----------------------------------------------------------------------
      I = 1
C                                       While (I < N) and (Not Found) do
20    IF ((I.GT.NPARS).OR.(LKEY.EQ.LPARS(I))) GO TO 50
         I = I + 1
         GO TO 20
C
50    IF (I.GT.NPARS) THEN
         WRITE (MSGTXT,1000) LKEY
         CALL MSGWRT (8)
         STOP
         END IF
C                                       Exit
      IKEY = I
      RETURN
C----------------------------------------------------------------------
1000  FORMAT ('FATAL ERROR: CONTACT AIPS ADMIN; LKEY=',A8)
      END
      SUBROUTINE TBCONS (IRET)
C----------------------------------------------------------------------
C   Consolidate the TY, GC and BL tables
C   Outputs:
C      IRET    I    Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANTAB.INC'
      INCLUDE 'INCS:DCAT.INC'
C----------------------------------------------------------------------
      IRET = 0
C                                       Consolidate the GC table
      IF (WGAIN) THEN
C                                       Gain table entries were written
         CALL GCCONS (IRET)
      ELSE IF (IGCVIN.EQ.0) THEN
C                                       Null GC table was created
         CALL RMEXT (INDISK, ICNO, 'GC', IGCVER, CATBLK, BUFF1, IRET)
         END IF
      IF (IRET.NE.0) GO TO 999
C                                       Consolidate TY table
      IF (WTSYS) THEN
C                                       TY entries were written
         CALL TYCONS (IRET)
      ELSE IF (ITYVIN.EQ.0) THEN
C                                       Null TY table was created
         CALL RMEXT (INDISK, ICNO, 'TY', ITYVER, CATBLK, BUFF1, IRET)
         END IF
      IF (IRET.NE.0) GO TO 999
C                                       Consolidate BL table
      IF (WBFAC) CALL BLCONS (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Exit
999   RETURN
      END
      SUBROUTINE ANTBHI (IRET)
C----------------------------------------------------------------------
C   Subroutine to update the history file
C   Output:
C      IRET    I    Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANTAB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      LOGICAL WUPDAT
      CHARACTER LHIREC*72, LTIME*20
      INTEGER IERR, LDATE(3), ITIME(3)
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
      CALL ZDATE (LDATE)
      CALL ZTIME (ITIME)
      CALL TIMDAT (ITIME, LDATE, LTIME(13:20), LTIME(1:12))
      WRITE (LHIREC,1010) TSKNAM, RLSNAM, LTIME(1:12), LTIME(13:20)
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       UV-file name
      WRITE (LHIREC,1020) TSKNAM, LNAME, LCLASS, INDISK, INSEQ
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Calibration file name
      WRITE (LHIREC,1040) TSKNAM, LINFIL
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Subarray
      WRITE (LHIREC,1060) TSKNAM, ISUBA
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       TYVER, GCVER, BLVER
      WRITE (LHIREC,1080) TSKNAM, ITYVER, IGCVER, IBLVER
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
1000  FORMAT ('ANTBHI: ERROR',I3,' OPENING HI TABLE')
1010  FORMAT (A6,' RELEASE: ',A8,' START TIME: ',A12,2X,A8)
1020  FORMAT (A6,' INNAME= ',A12,'.',A6,'.',I3,'.',I4)
1040  FORMAT (A6,' INFILE= ',A48)
1060  FORMAT (A6,' SUBARRAY= ',I4)
1080  FORMAT (A6,' TYVER= ',I4,' GCVER= ',I4,' BLVER= ',I4)
1980  FORMAT ('ANTBHI: ERROR',I3,' PROCESSING HISTORY FILE')
      END
      SUBROUTINE POLADJ (POLIN, POLOUT, N, OFFSET)
C-----------------------------------------------------------------------
C   Adjust 1-D polynomial coefficients for x-axis offset
C   Inputs:
C      POLIN   R(*) Input polynomial coefficients (ascending order)
C      N       I    Dimension of POLIN
C      OFFSET  R    Desired offset in x-abscissa
C   Outputs:
C      POLOUT  R(*) Transformed polynomial coefficients.
C-----------------------------------------------------------------------
      INTEGER N
      REAL POLIN(N), POLOUT(N), OFFSET
C
      DOUBLE PRECISION DSUM, DOFF, DCOMB
      INTEGER J, K, JJ, KK
C-----------------------------------------------------------------------
      DO 200 J = 1, N
         JJ = J - 1
         DSUM = DBLE (POLIN(J))
         DO 100 K = J+1, N
            KK = K - 1
            DOFF = DBLE (OFFSET)
            DSUM = DSUM + POLIN(K) * DCOMB (KK,JJ) * DOFF ** (KK-JJ)
100         CONTINUE
         POLOUT(J) = DSUM
200      CONTINUE
C                                       Exit
      RETURN
      END
      FUNCTION DCOMB (N, K)
C-----------------------------------------------------------------------
C   Coefficient for offset adjustment of polynomial coefficients
C   Inputs:
C      N       I   No of coefficients in series
C      K       I   (Order+1) of x term for which factor required
C   Output:
C      DCOMB   D   Factor for x**(k-1); combinatorial (N K)
C------------------------------------------------------------------------
      DOUBLE PRECISION DCOMB
      INTEGER N, K
C
      DOUBLE PRECISION DNUM, DENOM
      INTEGER I
C------------------------------------------------------------------------
      DCOMB = 0.0D0
      IF (K.GT.N) GO TO 999
      DNUM = 1.0D0
      DENOM = 1.0D0
C                                       Denominator
      DO 100 I = 1, K
         DENOM = DENOM * DBLE (I)
100      CONTINUE
C                                       Numerator
      DO 200 I = (1-K), 0
         DNUM = DNUM * DBLE (N + I)
200      CONTINUE
C
      IF (DENOM.NE.0.0D0) THEN
         DCOMB = DNUM / DENOM
      ELSE
         DCOMB = 1.0D0
         END IF
C                                       Exit
999   RETURN
      END
      SUBROUTINE DECOD (LINDX, NINDX, IPOLUV, NPOLUV, NIFUV, IMAP,
     *   NMAP, NCOL, IRET)
C------------------------------------------------------------------------
C   Subroutine to decode the INDEX or INDEX2 keyword strings
C   Inputs:
C      LINDX   C(*)*8   Input keyword string
C      NINDX   I        Dimension of LINDX
C      IPOLUV  I        Start polarization allowed (1=R; 2=L)
C      NPOLUV  I        No. of polarizations (1 or 2)
C      NIFUV   I        Maximum IF number allowed
C      NMAP    I        Dimension of IMAP
C   Outputs:
C      IMAP    I(2,*)   Mapping of polzn./IF to column no.
C      NCOL    I        No. of columns in keyword string
C      IRET    I        Return code (0 => ok)
C------------------------------------------------------------------------
      INTEGER NINDX, IPOLUV, NPOLUV, NIFUV, NMAP, NCOL, IRET
      CHARACTER*8 LINDX(NINDX)
      INTEGER IMAP(2,NMAP)
C
      INCLUDE 'INCS:DMSG.INC'
      INTEGER IENTRY
C------------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
      NCOL = 0
      IENTRY = 1
C                                       While (Entry_no < Nindx) and
C                                         (Not Error) do
 50   IF ((IENTRY.GT.NINDX).OR.(IRET.NE.0)) GO TO 500
C                                       Skip blank entries
         IF (LINDX(IENTRY).EQ.'        ') GO TO 200
         NCOL = IENTRY
C                                       Parse and update IMAP
         CALL PARSE (LINDX(IENTRY), IPOLUV, NPOLUV, NIFUV, IENTRY,
     *      IMAP, NMAP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1500)
            CALL MSGWRT (8)
            END IF
C
200      IENTRY = IENTRY + 1
         GO TO 50
C                                       Endwhile
500   CONTINUE
C
      RETURN
C-------------------------------------------------------------------------
1500  FORMAT ('DECOD: ERROR DECODING INDEX KEYWORD')
      END
      SUBROUTINE PARSE (LSTR, IPOLUV, NPOLUV, NIFUV, IENTRY, IMAP,
     *   NMAP, IRET)
C-----------------------------------------------------------------------
C   Subroutine to parse each INDEX or INDEX2 entry and update IMAP
C   Inputs:
C      LSTR     C*8    Input string
C      IPOLUV   I      Lowest allowed polarization (R=1; L=2)
C      NPOLUV   I      Max. no. of polzns. in table (1 or 2)
C      NIFUV    I      Max. no. IF's in data
C      IENTRY   I      Entry no. in INDEX keyword string
C      NMAP     I      Dimension of IMAP
C   Outputs:
C      IMAP     I(2,*) Map of polzn./IF to column no. in cal file
C      IRET     I      Return code (0 => ok)
C-----------------------------------------------------------------------
      CHARACTER LSTR*8
      INTEGER IPOLUV, NPOLUV, NIFUV, IENTRY, NMAP, IRET
      INTEGER IMAP(2,NMAP)
C
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WPROC, WVALID
      CHARACTER LPOPT(2), LOPER, LCHAR
      INTEGER JVALS(3), JPTR, J, NUMBER, ITYPE, K, IERR
      DATA LPOPT /'R', 'L'/
C-----------------------------------------------------------------------
C                                       Parse the entry
      J = 1
      IRET = 0
C                                       Reset stacks
      LOPER = ' '
      JPTR = 0
C                                       While (Not End_of_string) and
C                                             (Not Error) do
50    IF ((J.GT.8).OR.(IRET.NE.0)) GO TO 500
C                                       Get next symbol
         CALL GETSYM (LSTR, J, NUMBER, LCHAR, ITYPE)
C                                       End of string ?
         WPROC = (J.GT.8)
C                                       Case symbol_type of:
C                                       1: Character
         IF (ITYPE.NE.1) GO TO 200
C                                       Case Character of:
C                                       '|': process sub-string
            IF (LCHAR.NE.'|') GO TO 100
               WPROC = .TRUE.
               GO TO 180
C                                       ':': skip
100         IF (LCHAR.NE.':') GO TO 120
               GO TO 180
C                                       'R','L','X': valid operands
120         WVALID = .FALSE.
            DO 130 K = IPOLUV, (IPOLUV+NPOLUV-1)
               IF (LCHAR.EQ.LPOPT(K)) WVALID = .TRUE.
130            CONTINUE
            WVALID = (WVALID.OR.(LCHAR.EQ.'X'))
C
            IF (.NOT.WVALID) GO TO 150
C                                       Add to operand stack (should
C                                       not contain anything yet)
            IF (LOPER.NE.' ') THEN
               IRET = 3
               WRITE (MSGTXT,1130) LSTR
            ELSE
               LOPER = LCHAR
C                                       Reset number stack
               JPTR = 0
               END IF
C
            GO TO 180
C                                       Other character: error
150         IRET = 1
            WRITE (MSGTXT,1150) LCHAR, LSTR
            GO TO 180
C                                       Endcase (Character)
180         GO TO 350
C                                       2: Number; add to stack
200      IF (ITYPE.NE.2) GO TO 350
            JPTR = JPTR + 1
            IF (JPTR.GT.3) THEN
               IRET = 2
               WRITE (MSGTXT,1200) LSTR
               END IF
            IF (IRET.EQ.0) JVALS(JPTR) = NUMBER
            GO TO 350
C                                       Endcase (Symbol_type)
350      CONTINUE
C                                       Time to process the stacks ?
      IF (WPROC.AND.(IRET.EQ.0)) CALL PROCES (LOPER, JVALS, JPTR,
     *   IMAP, NMAP, NIFUV, IENTRY, IERR)
      IF (IERR.NE.0) THEN
         IRET = 9
C                                       Describe the error type
         IF (IERR.EQ.1) WRITE (MSGTXT,1351) LSTR
         IF (IERR.EQ.2) WRITE (MSGTXT,1352) LSTR
         IF (IERR.EQ.3) WRITE (MSGTXT,1353) LSTR
         IF (IERR.EQ.4) WRITE (MSGTXT,1354) LSTR
         IF (IERR.EQ.5) WRITE (MSGTXT,1355) LSTR
         END IF
C
      GO TO 50
C                                       Endwhile
500   IF (IRET.NE.0) CALL MSGWRT (8)
      RETURN
C-----------------------------------------------------------------------
1130  FORMAT ('PARSE: DOUBLE OPERAND: ',A8)
1150  FORMAT ('PARSE: INVALID CHARACTER (OR STOKES): ',A1,' IN: ',A8)
1200  FORMAT ('PARSE: TOO MANY NUMBERICAL ARGUMENTS: ',A8)
1351  FORMAT ('PARSE: NO ARGUMENT EXPECTED FOR X OPERAND: ',A8)
1352  FORMAT ('PARSE: UNIDENTIFIED OPERAND: ',A8)
1353  FORMAT ('PARSE: NO IF RANGE SPECIFIED: ',A8)
1354  FORMAT ('PARSE: IFNO. OUT OF RANGE: ',A8)
1355  FORMAT ('PARSE: NON-UNIQUE MAPPING: ',A8)
      END
      SUBROUTINE PROCES (LOPER, JVALS, JPTR, IMAP, NMAP, NIFUV,
     *   IENTRY, IRET)
C-----------------------------------------------------------------------
C   Process the operand and number stacks created by PARSE
C   Inputs:
C      JVALS   I(3)   Number stack (three deep)
C      NMAP    I      Dimension of IMAP
C      NIFUV   I      Max. no. of allowed IF's
C      IENTRY  I      Entry no. in INDEX keyword list
C   Input/output:
C      LOPER   C*1    Operand stack (one deep)
C      JPTR    I      Pointer to JVALS
C   Outputs:
C      IMAP    I(2,*) Map of polzn./IF to column number in cal. file
C      IRET    I      Return code (0 => ok)
C-----------------------------------------------------------------------
      CHARACTER LOPER
      INTEGER NMAP, IMAP(2,NMAP), JVALS(3), NIFUV, IENTRY, JPTR, IRET
C
      INTEGER KPOL, ISTART, IEND, INCIF, K
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Case Operand of:
C                                       'X':
      IF (LOPER.NE.'X') GO TO 100
C                                       No number argument expected
         IF (JPTR.NE.0) IRET = 1
         GO TO 450
C                                       'R','L': process
100   IF ((LOPER.NE.'R').AND.(LOPER.NE.'L')) GO TO 200
C                                       Check the number stack
         IF (JPTR.GT.0) THEN
            IF (JPTR.EQ.1) THEN
C                                       eg. 'R1'
               ISTART = JVALS(1)
               IEND = ISTART
               INCIF = 1
               END IF
            IF (JPTR.EQ.2) THEN
C                                       eg. 'R1:2'
               ISTART = JVALS(1)
               IEND = JVALS(2)
               INCIF = 1
               END IF
            IF (JPTR.EQ.3) THEN
C                                       eg. 'R10:16:2'
               ISTART = JVALS(1)
               IEND = JVALS(2)
               INCIF = JVALS(3)
               END IF
C                                       Check IF ranges
            IF ((ISTART.GT.IEND).OR.(IEND.GT.NIFUV)) IRET = 4
            IF (IRET.GT.0) GO TO 450
C                                       Update IMAP
            IF (LOPER.EQ.'R') KPOL = 1
            IF (LOPER.EQ.'L') KPOL = 2
            DO 170 K = ISTART, IEND, INCIF
               IF ((IMAP(KPOL,K).NE.IENTRY).AND.(IMAP(KPOL,K).NE.0))
     *            IRET = 5
               IMAP(KPOL,K) = IENTRY
170            CONTINUE
C
         ELSE
C                                       Number stack is empty
            IRET = 3
            END IF
         GO TO 450
C                                       Unidentified operand: error
200    IRET = 2
       GO TO 450
C                                       Endcase
450   CONTINUE
C                                       Reset stacks
      LOPER = ' '
      JPTR = 0
C
      RETURN
      END
      SUBROUTINE GETSYM (LSTR, I, NUM, LCHAR, ITYPE)
C-----------------------------------------------------------------------
C   Get the next symbol from an input string
C   Inputs:
C      LSTR   C*8   Input string to search
C   Input/output:
C      I      I     Current pointer in LSTR
C   Output:
C      NUM    I     Numerical value of symbol
C      LCHAR  C*1   Character symbol
C      ITYPE  I     Symbol type (1=character; 2=number; 0=none found)
C-----------------------------------------------------------------------
      CHARACTER LSTR*8, LCHAR
      INTEGER I, NUM, ITYPE
C
      LOGICAL WEND
      CHARACTER LDIGIT(10)
      INTEGER J, K
      DATA LDIGIT /'0','1','2','3','4','5','6','7','8','9'/
C----------------------------------------------------------------------
      NUM = 0
      LCHAR = ' '
      ITYPE = 0
      WEND = .FALSE.
C                                       Repeat
C                                        Case (Character) of:
C                                        SPACE: skip
50    IF (LSTR(I:I).NE.' ') GO TO 100
         GO TO 300
C                                        CHARACTER:
100   J = 0
      DO 120 K = 1, 10
         IF (LSTR(I:I).EQ.LDIGIT(K)) J = K
120      CONTINUE
      IF (J.NE.0) GO TO 150
C
         IF (ITYPE.EQ.2) THEN
            WEND = .TRUE.
            I = I - 1
         ELSE
            LCHAR = LSTR(I:I)
            ITYPE = 1
            END IF
         GO TO 300
C                                        DIGIT:
150   NUM = 10 * NUM + (J - 1)
      ITYPE = 2
      GO TO 300
C                                        Endcase
300   CONTINUE
C                                       Until (end_of_string) or
C                                          (character_found) or
C                                          (end_of_number)
      I = I + 1
      IF ((I.LE.8).AND.(ITYPE.NE.1).AND.(.NOT.WEND)) GO TO 50
C                                       Exit
      RETURN
      END
      SUBROUTINE DAYYR (IYR, IMONTH, IDAY, IDAYNO, ILOYR)
C-----------------------------------------------------------------------
C   Subroutine to compute day number (Jan 1 = 1) from Gregorian date
C   Inputs:
C      IYR     I   Year number (eg. 1994)
C      IMONTH  I   Month number (Jan=1, Feb=2,.., Dec=12)
C      IDAY    I   Day of month
C   Outputs:
C      IDAYNO  I   Day number (Jan. 1 = 1)
C      ILOYR   I   No. of days in year (365 or 366)
C----------------------------------------------------------------------
      INTEGER IYR, IMONTH, IDAY, IDAYNO, ILOYR
C
      LOGICAL WLEAP
      INTEGER IDSUM(12)
      DATA IDSUM /0,31,59,90,120,151,181,212,243,273,304,334/
C----------------------------------------------------------------------
      IDAYNO = IDSUM(IMONTH) + IDAY
      ILOYR = 365
C                                       Leap year ?
      WLEAP = (MOD(IYR,4).EQ.0)
C                                       Centurial leap years ?
      IF (WLEAP.AND.(MOD(IYR,100).EQ.0)) WLEAP = (MOD(IYR,400).EQ.0)
C
      IF (WLEAP.AND.(IMONTH.GT.2)) IDAYNO = IDAYNO + 1
      IF (WLEAP) ILOYR = 366
      RETURN
      END
      SUBROUTINE NXSRCH (RTIME, ISOUID, IFQID)
C----------------------------------------------------------------------
C   Search the NX table for the source and freq. ID. at a given time
C   Inputs:
C      RTIME   R   Input time relative to the ref. date (in days)
C   Outputs:
C      ISOUID  I   Source id. (0 if not found)
C      IFQID   I   FQ. id.
C---------------------------------------------------------------------
      REAL RTIME
      INTEGER ISOUID, IFQID
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANTAB.INC'
      INTEGER I
C---------------------------------------------------------------------
      ISOUID = 0
      IFQID = 0
C                                       Search the NX table
      DO 100 I = 1, NXDAT
         IF ((RTIME .GE. (TIMENX(1,I) - DELTAT)) .AND.
     *      (RTIME .LE.  (TIMENX(2,I) + DELTAT))) THEN
            ISOUID = INXSOU(I)
            IFQID = INXFQ(I)
            END IF
100      CONTINUE
C                                       Exit
      RETURN
      END
      SUBROUTINE GCCONS (IRET)
C----------------------------------------------------------------------
C   Consolidate the GC table; later entries overwrite previous entries.
C   Output:
C      IRET    I    Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PGCV.INC'
      INCLUDE 'ANTAB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER MXKINX
      PARAMETER (MXKINX = 1024)
      REAL XVALGC(2,MAXIF), YVALGC(2,MAXIF,MXTBGC),
     *   GAINGC(2,MAXIF,MXTBGC), SENSGC(2,MAXIF), XVALG2(2,MAXIF),
     *   YVALG2(2,MAXIF,MXTBGC), GAING2(2,MAXIF,MXTBGC),
     *   SENSG2(2,MAXIF)
      INTEGER IGCRNO, GCKOLS(MAXGCC), GCNUMV(MAXGCC), NPOLGC, NTABGC,
     *   NOBAND, IERR, IANTGC, ISUBGC, IFQDGC,
     *   ITPGC(2,MAXIF), NTGC(2,MAXIF), IXTGC(2,MAXIF), IYTGC(2,MAXIF),
     *   IANTG2, ISUBG2, IFQDG2, ITPG2(2,MAXIF), NTG2(2,MAXIF),
     *   IXTG2(2,MAXIF), IYTG2(2,MAXIF), IGCRN2, MSGSAV
      INTEGER NKINDX, KINDX(MXKINX), I, J, NROW, JPOL, JIF, M
C----------------------------------------------------------------------
      IRET = 0
C                                       Open input GC table
      CALL GCINI ('READ', BUFF1, INDISK, ICNO, IGCVER, CATBLK, ILUN1,
     *   IGCRNO, GCKOLS, GCNUMV, NPOLGC, NOBAND, NTABGC, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C
      NKINDX = 0
      NROW = BUFF1(5)
C                                       Check table size
      IF (MXKINX.LT.NROW) THEN
         IRET = 6
         WRITE (MSGTXT,1030)
         GO TO 990
         END IF
C                                       Compile index
      DO 100 I = 1, NROW
         IGCRNO = I
         CALL TABGC ('READ', BUFF1, IGCRNO, GCKOLS, GCNUMV, NPOLGC,
     *      NTABGC, IANTGC, ISUBGC, IFQDGC, ITPGC, NTGC, IXTGC,
     *      IYTGC, XVALGC, YVALGC, GAINGC, SENSGC, IERR)
         IF (IERR.EQ.-1) GO TO 100
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Add to keyword index
         NKINDX = NKINDX + 1
         KINDX(NKINDX) = (ISUBGC - 1) * MAXANT * MXFQID +
     *      (IFQDGC - 1) * MAXANT + IANTGC
100      CONTINUE
C                                       Open output table
      IGCTMP = 0
      CALL GCINI ('WRIT', BUFF2, INDISK, ICNO, IGCTMP, CATBLK, ILUN2,
     *   IGCRN2, GCKOLS, GCNUMV, NPOLGC, NOBAND, NTABGC, IERR)
      IF (IERR.NE.0) THEN
         IRET = 3
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Write consolidated table
      DO 300 I = 1,NKINDX
C                                       Skip records already processed
         IF (KINDX(I).LE.0) GO TO 300
C                                       Read record
         IGCRNO = I
         CALL TABGC ('READ', BUFF1, IGCRNO, GCKOLS, GCNUMV, NPOLGC,
     *      NTABGC, IANTGC, ISUBGC, IFQDGC, ITPGC, NTGC, IXTGC, IYTGC,
     *      XVALGC, YVALGC, GAINGC, SENSGC, IERR)
         IF (IERR.EQ.-1) GO TO 300
         IF (IERR.NE.0) THEN
            IRET = 4
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C
         DO 200 J = (I+1), NKINDX
C                                       Look for records with
C                                       matching keywords
            IF (KINDX(J).EQ.KINDX(I)) THEN
C                                       Read matching record
               IGCRNO = J
               CALL TABGC ('READ', BUFF1, IGCRNO, GCKOLS, GCNUMV,
     *            NPOLGC, NTABGC, IANTG2, ISUBG2, IFQDG2, ITPG2, NTG2,
     *            IXTG2, IYTG2, XVALG2, YVALG2, GAING2, SENSG2, IERR)
               IF (IERR.EQ.-1) GO TO 200
               IF (IERR.NE.0) THEN
                  IRET = 4
                  WRITE (MSGTXT,1050) IERR
                  GO TO 990
                  END IF
C                                       Consolidate the two records
               DO 150 JPOL = 1,NPOLGC
                  DO 120 JIF = 1,NOBAND
C                                       Skip null records
                     IF (ITPG2(JPOL,JIF).EQ.0) GO TO 120
C
                     IF ((ITPGC(JPOL,JIF).EQ.0).OR.
     *                  ((ITPG2(JPOL,JIF).EQ.ITPGC(JPOL,JIF)).AND.
     *                  ((IXTG2(JPOL,JIF).EQ.IXTGC(JPOL,JIF)).AND.
     *                  (XVALG2(JPOL,JIF).EQ.XVALGC(JPOL,JIF))))) THEN
C                                       Overwrite the earlier record
                        ITPGC(JPOL,JIF) = ITPG2(JPOL,JIF)
                        NTGC(JPOL,JIF) = NTG2(JPOL,JIF)
                        IXTGC(JPOL,JIF) = IXTG2(JPOL,JIF)
                        IYTGC(JPOL,JIF) = IYTG2(JPOL,JIF)
                        XVALGC(JPOL,JIF) = XVALG2(JPOL,JIF)
                        SENSGC(JPOL,JIF) = SENSG2(JPOL,JIF)
                        DO 115 M = 1, NTABGC
                           YVALGC(JPOL,JIF,M) = YVALG2(JPOL,JIF,M)
                           GAINGC(JPOL,JIF,M) = GAING2(JPOL,JIF,M)
115                        CONTINUE
C                                       Mark 2nd record as processed
                        KINDX(J) = 0
                        END IF
120                  CONTINUE
150               CONTINUE
C
               END IF
200         CONTINUE
C                                       Write output record
            CALL TABGC ('WRIT', BUFF2, IGCRN2, GCKOLS, GCNUMV, NPOLGC,
     *         NTABGC, IANTGC, ISUBGC, IFQDGC, ITPGC, NTGC, IXTGC,
     *         IYTGC, XVALGC, YVALGC, GAINGC, SENSGC, IERR)
            IF (IERR.NE.0) THEN
               IRET = 5
               WRITE (MSGTXT,1200) IERR
               GO TO 990
               END IF
C
300      CONTINUE
C                                       Close the tables
      CALL TABIO ('CLOS', 0, IGCRNO, BUFF1, BUFF1, IERR)
      CALL TABIO ('CLOS', 0, IGCRN2, BUFF2, BUFF2, IERR)
C                                       Delete the old table
      CALL RMEXT (INDISK, ICNO, 'GC', IGCVER, CATBLK, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 7
         WRITE (MSGTXT,1300) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Copy new to old
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('GC', IGCTMP, IGCVER, ILUN1, ILUN2, INDISK, INDISK,
     *   ICNO, ICNO, CATBLK, BUFF1, BUFF2, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         IRET = 8
         WRITE (MSGTXT,1310) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Delete 2nd copy of new table
      CALL RMEXT (INDISK, ICNO, 'GC', IGCTMP, CATBLK, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 7
         WRITE (MSGTXT,1300) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Close tables
      IF (IRET.NE.1) CALL TABIO ('CLOS', 0, IGCRNO, BUFF1, BUFF1, IERR)
      IF (IRET.NE.3) CALL TABIO ('CLOS', 0, IGCRN2, BUFF2, BUFF2, IERR)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1020  FORMAT ('GCCONS: ERROR',I3,' OPENING GC TABLE')
1030  FORMAT ('GCCONS: CONTACT AIPS ADMIN: PARAMETER MXKINX TOO SMALL')
1050  FORMAT ('GCCONS: ERROR',I3,' READING GC TABLE')
1200  FORMAT ('GCCONS: ERROR',I3,' WRITING GC TABLE')
1300  FORMAT ('GCCONS: ERROR',I3,' DELETING TY TABLE')
1310  FORMAT ('GCCONS: ERROR',I3,' COPYING TY TABLE')
      END
      SUBROUTINE TYCONS (IRET)
C----------------------------------------------------------------------
C   Consolidate the TY table; new entries overwrite older ones.
C   Outputs:
C      IRET    I    Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANTAB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER MXKINX
      PARAMETER (MXKINX = 200000)
      LOGICAL WMATCH, WTYRAT, WTYSYS, WTYANT
      REAL TIMETY, TINTTY, TSYST(2,MAXIF), TANT(2,MAXIF), TIMET2,
     *   TINTT2, TSYST2(2,MAXIF), TANT2(2,MAXIF), TIMNDX(MXKINX)
      INTEGER ITYRNO, ITYRN2, TYKOLS(MAXTYC), TYNUMV(MAXTYC), NPOLTY,
     *   NIFTY, IERR, ITYSOU, ITYANT, ITYSUB, ITYFQD, IT2SOU, IT2ANT,
     *   IT2SUB, IT2FQD, MSGSAV
      INTEGER NKINDX, KINDX(MXKINX), INDXSU(MXKINX), I, J, JPOL, JIF,
     *   NROW
C----------------------------------------------------------------------
      IRET = 0
C                                       Open input TY table
      CALL TYINI ('READ', BUFF1, INDISK, ICNO, ITYVER, CATBLK, ILUN1,
     *   ITYRNO, TYKOLS, TYNUMV, NPOLTY, NIFTY, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C
      NROW = BUFF1(5)
      NKINDX = 0
C                                       Check table size
      IF (MXKINX.LT.NROW) THEN
         IRET = 6
         WRITE (MSGTXT,1030)
         GO TO 990
         END IF
C                                       Compile index
      DO 100 I = 1, NROW
         ITYRNO = I
         CALL TABTY ('READ', BUFF1, ITYRNO, TYKOLS, TYNUMV, NPOLTY,
     *      NIFTY, TIMETY, TINTTY, ITYSOU, ITYANT, ITYSUB, ITYFQD,
     *      TSYST, TANT, IERR)
         IF (IERR.EQ.-1) GO TO 100
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
C                                       Add to keyword index
         NKINDX = NKINDX + 1
         KINDX(NKINDX) = (ITYSUB - 1) * MAXANT * MXFQID +
     *      (ITYFQD - 1) * MAXANT + ITYANT
         TIMNDX(NKINDX) = TIMETY
         INDXSU(NKINDX) = ITYSOU
100      CONTINUE
C                                       Open output table
      ITYTMP = 0
      CALL TYINI ('WRIT', BUFF2, INDISK, ICNO, ITYTMP, CATBLK, ILUN2,
     *   ITYRN2, TYKOLS, TYNUMV, NPOLTY, NIFTY, IERR)
      IF (IERR.NE.0) THEN
         IRET = 3
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Write consolidated table
      DO 300 I = 1, NKINDX
C                                       Skip records already processed
         IF (KINDX(I).LE.0) GO TO 300
C                                       Read record
         ITYRNO = I
         CALL TABTY ('READ', BUFF1, ITYRNO, TYKOLS, TYNUMV, NPOLTY,
     *      NIFTY, TIMETY, TINTTY, ITYSOU, ITYANT, ITYSUB, ITYFQD,
     *      TSYST, TANT, IERR)
         IF (IERR.EQ.-1) GO TO 300
         IF (IERR.NE.0) THEN
            IRET = 4
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
C
         DO 200 J = (I+1), NKINDX
C                                       Look for matching keywords
            WMATCH = ((KINDX(I).EQ.KINDX(J)).AND.(TIMNDX(J).EQ.
     *         TIMNDX(I)).AND.(INDXSU(I).EQ.INDXSU(J)))
            IF (WMATCH) THEN
C                                       Read matching record
               ITYRNO = J
               CALL TABTY ('READ', BUFF1, ITYRNO, TYKOLS, TYNUMV,
     *            NPOLTY, NIFTY, TIMET2, TINTT2, IT2SOU, IT2ANT,
     *            IT2SUB, IT2FQD, TSYST2, TANT2, IERR)
               IF (IERR.EQ.-1) GO TO 200
               IF (IERR.NE.0) THEN
                  IRET = 4
                  WRITE (MSGTXT,1100) IERR
                  GO TO 990
                  END IF
C                                       Consolidate the two records
               DO 150 JPOL = 1, NPOLTY
                  DO 120 JIF = 1, NIFTY
C                                       Skip blank records
                     IF ((TSYST2(JPOL,JIF).EQ.FBLANK).AND.
     *                  ((TANT2(JPOL,JIF).EQ.FBLANK).OR.
     *                  (TANT2(JPOL,JIF).LT.0.0))) GO TO 120
C                                       Determine record type
                     WTYRAT = ((TSYST2(JPOL,JIF).NE.FBLANK).AND.
     *                  (TANT2(JPOL,JIF).LT.0.0))
                     WTYSYS = ((TSYST2(JPOL,JIF).NE.FBLANK).AND.
     *                  (TANT2(JPOL,JIF).EQ.FBLANK))
                     WTYANT = ((TSYST2(JPOL,JIF).EQ.FBLANK).AND.
     *                  (TANT2(JPOL,JIF).NE.FBLANK))
C                                       New record is Ta/Tsys
                     IF (WTYRAT) THEN
                        TSYST(JPOL,JIF) = TSYST2(JPOL,JIF)
                        TANT(JPOL,JIF) = TANT2(JPOL,JIF)
                        END IF
C                                       New record is Tsys
                     IF (WTYSYS) THEN
                        TSYST(JPOL,JIF) = TSYST2(JPOL,JIF)
                        IF (TANT(JPOL,JIF).LT.0.0)
     *                     TANT(JPOL,JIF) = FBLANK
                        END IF
C                                       New record is Tant
                     IF (WTYANT) THEN
                        IF (TANT(JPOL,JIF).LT.0.0)
     *                     TSYST(JPOL,JIF) = FBLANK
                        TANT(JPOL,JIF) = TANT2(JPOL,JIF)
                        END IF
120                  CONTINUE
150               CONTINUE
C                                       Mark 2nd record as processed
               KINDX(J) = 0
C
               END IF
200         CONTINUE
C                                       Write output record
         CALL TABTY ('WRIT', BUFF2, ITYRN2, TYKOLS, TYNUMV, NPOLTY,
     *      NIFTY, TIMETY, TINTTY, ITYSOU, ITYANT, ITYSUB, ITYFQD,
     *      TSYST, TANT, IERR)
         IF (IERR.NE.0) THEN
            IRET = 5
            WRITE (MSGTXT,1200) IERR
            GO TO 990
            END IF
C
300   CONTINUE
C                                       Close the tables
      CALL TABIO ('CLOS', 0, ITYRNO, BUFF1, BUFF1, IERR)
      CALL TABIO ('CLOS', 0, ITYRN2, BUFF2, BUFF2, IERR)
C                                       Delete the old table
      CALL RMEXT (INDISK, ICNO, 'TY', ITYVER, CATBLK, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 7
         WRITE (MSGTXT,1300) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Copy new to old
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('TY', ITYTMP, ITYVER, ILUN1, ILUN2, INDISK,
     *   INDISK, ICNO, ICNO, CATBLK, BUFF1, BUFF2, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         IRET = 8
         WRITE (MSGTXT,1310) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Delete 2nd copy of new table
      CALL RMEXT (INDISK, ICNO, 'TY', ITYTMP, CATBLK, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 7
         WRITE (MSGTXT,1300) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
      IF (IRET.NE.1) CALL TABIO ('CLOS', 0, ITYRNO, BUFF1, BUFF1, IERR)
      IF (IRET.NE.3) CALL TABIO ('CLOS', 0, ITYRN2, BUFF2, BUFF2, IERR)
C                                       Exit
999   RETURN
C---------------------------------------------------------------------
1020  FORMAT ('TYCONS: ERROR',I3,' OPENING TY TABLE')
1030  FORMAT ('TYCONS: CONTACT AIPS ADMIN: PARAMETER MXKINX TOO SMALL')
1100  FORMAT ('TYCONS: ERROR',I3,' READING TY TABLE')
1200  FORMAT ('TYCONS: ERROR',I3,' WRITING TY TABLE')
1300  FORMAT ('TYCONS: ERROR',I3,' DELETING TY TABLE')
1310  FORMAT ('TYCONS: ERROR',I3,' COPYING TY TABLE')
      END
      SUBROUTINE BLCONS (IRET)
C---------------------------------------------------------------------
C   Consolidate the BL table; newer entries overwrite older ones
C   Outputs:
C      IRET    I     Return code (0 => ok)
C---------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANTAB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      REAL FACMUL(2,2,MAXIF), FACADD(2,2,MAXIF), BLTIME, BLFAC
      INTEGER IBLRNO, BLNUMV(MAXBLC), BLKOLS(MAXBLC), IERR, NPOLBL,
     *   NIFBL, IBLSOU, IBLFQD, NROW, I, IBLANT, JBLANT, JIF, JPOL,
     *   NDIM, NANTBL, IBLSUB, JFQID, J
C--------------------------------------------------------------------
      IRET = 0
C                                       Open input BL table
      NPOLBL = NPOLUV
      NIFBL = NIFUV
      NANTBL = NSTNS
      CALL BLINI ('WRIT', BUFF1, INDISK, ICNO, IBLVER, CATBLK, ILUN1,
     *   IBLRNO, BLKOLS, BLNUMV, NANTBL, NPOLBL, NIFBL, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C
      NROW = BUFF1(5)
      DO 100 I = 1, NROW
C                                       Read record
         IBLRNO = I
         CALL TABBL ('READ', BUFF1, IBLRNO, BLKOLS, BLNUMV, NPOLBL,
     *      BLTIME, IBLSOU, IBLSUB, IBLANT, JBLANT, IBLFQD,
     *      FACMUL, FACADD, IERR)
         IF (IERR.EQ.-1) GO TO 100
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Is there an entry for this
C                                       baseline ?
         BLFAC = BFAC(IBLANT,JBLANT)
         IF (BLFAC.GT.0.0) THEN
            DO 50 JIF = 1, NIFBL
               DO 40 JPOL = 1, NPOLBL
                  FACMUL(1,JPOL,JIF) = BLFAC
                  FACMUL(2,JPOL,JIF) = 0.0
40                CONTINUE
50             CONTINUE
C                                       Mark as used
            BFAC(IBLANT,JBLANT) = 0.0
C                                       Re-write record
            IBLRNO = I
            CALL TABBL ('WRIT', BUFF1, IBLRNO, BLKOLS, BLNUMV, NPOLBL,
     *         BLTIME, IBLSOU, IBLSUB, IBLANT, JBLANT, IBLFQD, FACMUL,
     *         FACADD, IERR)
            IF (IERR.NE.0) THEN
               IRET = 3
               WRITE (MSGTXT,1060) IERR
               GO TO 990
               END IF
            END IF
100      CONTINUE
C                                       Append new entries to end
C                                       of table
      IBLRNO = NROW + 1
      DO 300 I = 1, NSTNS
         IBLANT = TELNO(I)
         DO 200 J = 1, NSTNS
            JBLANT = TELNO(J)
            BLFAC = BFAC(IBLANT,JBLANT)
            IF (BLFAC.GT.0.0) THEN
C                                       Write a record for each FQID
               DO 180 JFQID = 1, NFQUV
                  IBLFQD = IFQUV(JFQID)
C                                       Initialise new BL record
                  NDIM = 4 * MAXIF
                  CALL RFILL (NDIM, 0.0, FACADD)
                  BLTIME = 0.0
                  IBLSOU = -1
                  IBLSUB = ISUBA
C                                       Loop over IF/polzn.
                  DO 150 JIF = 1, NIFBL
                     DO 140 JPOL = 1, NPOLBL
                        FACMUL(1,JPOL,JIF) = BLFAC
                        FACMUL(2,JPOL,JIF) = 0.0
140                     CONTINUE
150                  CONTINUE
C                                       Write new record
                  CALL TABBL ('WRIT', BUFF1, IBLRNO, BLKOLS, BLNUMV,
     *               NPOLBL, BLTIME, IBLSOU, IBLSUB, IBLANT, JBLANT,
     *               IBLFQD, FACMUL, FACADD, IERR)
                  IF (IERR.NE.0) THEN
                     IRET = 4
                     WRITE (MSGTXT,1060) IERR
                     GO TO 990
                     END IF
180               CONTINUE
               END IF
200         CONTINUE
300      CONTINUE
C                                       Close BL table
      CALL TABIO ('CLOS', 0, IBLRNO, BUFF1, BUFF1, IERR)
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C
999   IF (IRET.NE.0) CALL TABIO ('CLOS', 0, IBLRNO, BUFF1, BUFF1, IERR)
C                                       Exit
      RETURN
C------------------------------------------------------------------------
1020  FORMAT ('BLCONS: ERROR',I3,' OPENING BL TABLE')
1050  FORMAT ('BLCONS: ERROR',I3,' READING BL TABLE')
1060  FORMAT ('BLCONS: ERROR',I3,' WRITING BL TABLE')
      END
