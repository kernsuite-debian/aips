LOCAL INCLUDE 'BSPRT.INC'
C
C   Miscellaneous global data.
C
C   MODE      BS table solution mode ('INDE', 'VLBA', 'MK3 ' or 'RATE')
C   NUMIF     Number of logical IFs in the BS table
C             (1 <= NUMIF)
C   CURSRC    Source ID of current source (used to identify source
C             changes)
C   MAXSUB    The highest subarray number for the input data file
C   HAVPOL    Is the polarization with the Stokes code corresponding
C             to the index present
C
      CHARACTER MODE*4
      INTEGER   NUMIF, CURSRC, MAXSUB
      LOGICAL   HAVPOL(-8:-1)
C
      COMMON /NBSPRT/ NUMIF, CURSRC, MAXSUB, HAVPOL
      COMMON /CBSPRT/ MODE
      SAVE /NBSPRT/, /CBSPRT/
C
LOCAL END
      PROGRAM BSPRT
C-----------------------------------------------------------------------
C! Apply baseline-based fringe corrections.
C# UV Calibration Ext-appl Table Hardcopy VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 1996
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
C   BSPRT prints the contents of a BS table using a more organized
C   presentation than is possible with PRTAB.
C-----------------------------------------------------------------------
C
C   Local variables:
C
C   UVFILE    UVDATA object used to access the uv data file (parameter)
C   BSTAB     TABLE object used to access the input BS table (parameter)
C   OUTPRT    PRINTER object used to output report (parameter)
C   IRET      Error code (0 implies no errors have been detected)
C   DIEBUF    Scratch buffer for DIE
C
      CHARACTER UVFILE*12, BSTAB*8, OUTPRT*7
      INTEGER   IRET, DIEBUF(256)
C
      PARAMETER (UVFILE = 'uv data file')
      PARAMETER (BSTAB  = 'BS table')
      PARAMETER (OUTPRT = 'printer')
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
C-----------------------------------------------------------------------
      CALL SETUP (UVFILE, BSTAB, OUTPRT, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL PRNTBL (UVFILE, BSTAB, OUTPRT, IRET)
      IF (IRET .NE. 0) GO TO 999
C
      CALL PRTCLO (OUTPRT, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL TABCLO (BSTAB, IRET)
      IF (IRET .NE. 0) GO TO 999
C
  999 CALL DIE (IRET, DIEBUF)
      END
      SUBROUTINE SETUP (UVFILE, BSTAB, OUTPRT, IRET)
C-----------------------------------------------------------------------
C   Initialize the task and open the input table and the printer.
C
C   Inputs:
C      UVFILE    C*(*)    UVDATA object used to access the input file
C      BSTAB     C*(*)    TABLE object used to access the BS table
C      OUTPRT    C*(*)    PRINTER object used to print the report
C
C   Outputs:
C      IRET      I        Error code (0 implies no errors detected)
C
C   Preconditions:
C      UVFILE, BSTAB and OUTPRT are not blank and are unique
C
C   Postconditions:
C      IRET .EQ. 0 implies that
C         UVFILE is initialized and closed
C         BSTAB is open for reading and in time order
C         OUTPRT is open
C         MODE and NUMIF are set from BSTAB
C         CURSRC < 0
C         MAXSUB is set
C-----------------------------------------------------------------------
      CHARACTER UVFILE*(*), BSTAB*(*), OUTPRT*(*)
      INTEGER   IRET
C
C   Task description:
C
C   PRGN      Task name (parameter)
C   INPUTS    INPUTS object name (parameter)
C   NUMADV    Number of adverbs (parameter)
C   AVNAME    Adverb names
C   AVTYPE    Adverb types
C   AVDIM     Adverb types
C
      CHARACTER PRGN*6, INPUTS*6
      PARAMETER (PRGN = 'BSPRT ')
      PARAMETER (INPUTS = 'inputs')
      INTEGER   NUMADV
      PARAMETER (NUMADV = 7)
      CHARACTER AVNAME(NUMADV)*8
      INTEGER   AVTYPE(NUMADV), AVDIM(2, NUMADV)
C
C   UV data file
C
C   NKEY1     Number of adverbs to transfer to UVFILE (parameter)
C   INKEY1    Adverbs to transfer to UVFILE
C   OUTKY1    Attributes to receive adverb values
C
      INTEGER   NKEY1
      PARAMETER (NKEY1 = 4)
      CHARACTER INKEY1(NKEY1)*8, OUTKY1(NKEY1)*16
C
C   BS table
C
C   BSVER     Version number of BS table
C   BSROW     First row in BS table (ignored)
C   SORT      BS table sort order
C
      INTEGER   BSROW, BSVER, SORT(2)
C
C   Printer
C
C   NKEY2     Number of adverbs to transfer to OUTPRT
C   INKEY2    Adverbs to transfer to OUTPRT
C   OUTKY2    Attributes to receive adverb values
C
      INTEGER   NKEY2
      PARAMETER (NKEY2 = 2)
      CHARACTER INKEY2(NKEY2)*8, OUTKY2(NKEY2)*8
C
C   Other local variables
C
C   ANTAB     Temporary TABLE object for antenna files (parameter)
C   TYPE      AIPS attribute type code
C   DIM       AIPS attribute dimensions
C   CDUMMY    Placeholder
C
      CHARACTER ANTAB*8
      PARAMETER (ANTAB = 'an table')
      INTEGER   TYPE, DIM(3)
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSPRT.INC'
C
      DATA AVNAME /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *             'INVERS  ', 'DOCRT   ', 'OUTPRINT'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT, OOAINT,
     *   OOACAR/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 1,1, 1,1, 48,1/
C
      DATA INKEY1 /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  '/
      DATA OUTKY1 /'FILE_NAME.NAME  ', 'FILE_NAME.CLASS ',
     *             'FILE_NAME.IMSEQ ', 'FILE_NAME.DISK  '/
C
      DATA INKEY2 /'DOCRT   ', 'OUTPRINT'/
      DATA OUTKY2 /'DOCRT   ', 'LPFILE  '/
C-----------------------------------------------------------------------
      CALL AV2INP (PRGN, NUMADV, AVNAME, AVTYPE, AVDIM, INPUTS, IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Try opening the uv file to check that it exists and to initialize
C     descriptive information in common blocks:
C
      CALL OUVCRE (UVFILE, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL IN2OBJ (INPUTS, NKEY1, INKEY1, OUTKY1, UVFILE, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL OUVOPN (UVFILE, 'READ', IRET)
      IF (IRET .NE. 0) THEN
         MSGTXT = 'CAN NOT OPEN UV FILE - CHECK ADVERBS'
         CALL MSGWRT (9)
         GO TO 999
      END IF
      CALL OUVCLO (UVFILE, IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Find the highest AN table version number (this is the highest
C     subarray number):
C
      CALL UV2TAB (UVFILE, ANTAB, 'AN', 0, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL TBLHIV (ANTAB, MAXSUB, IRET)
      IF ((IRET .NE. 0) .OR. (MAXSUB .LE. 0)) THEN
         MSGTXT = 'CAN NOT DETERMINE THE NUMBER OF SUBARRAYS'
         CALL MSGWRT (9)
         MSGTXT = 'ANTENNA TABLES ARE MISSING'
         CALL MSGWRT (9)
         IRET = 999
         GO TO 999
      END IF
      CALL TABDES (ANTAB, IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Open the BS table and sort it into time order, if necessary:
C
      CALL INGET (INPUTS, 'INVERS', TYPE, DIM, BSVER, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL UV2TAB (UVFILE, BSTAB, 'BS', BSVER, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL OBSINI (BSTAB, 'READ', BSROW, MODE, NUMIF, IRET)
      IF (IRET .NE. 0) THEN
         MSGTXT = 'BS TABLE IS MISSING - CHECK ADVERBS'
         CALL MSGWRT (9)
         GO TO 999
      END IF
      CALL TABGET (BSTAB, 'SORT', TYPE, DIM, SORT, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
      IF (SORT(1) .NE. 1) THEN
         CALL TABCLO (BSTAB, IRET)
         IF (IRET .NE. 0) GO TO 999
         CALL TBLSRT (BSTAB, 'TIME    ', 'TIME    ', IRET)
         IF (IRET .NE. 0) GO TO 999
         CALL OBSINI (BSTAB, 'READ', BSROW, MODE, NUMIF, IRET)
         IF (IRET .NE. 0) GO TO 999
      END IF
C
C     Open the printer:
C
      CALL PRTCRE (OUTPRT, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL IN2OBJ (INPUTS, NKEY2, INKEY2, OUTKY2, OUTPRT, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL PRTOPN (OUTPRT, '    ', IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Set the current source number to an impossible value so that
C     source data will be read automatically on encountering the
C     first source in the BS table.
C
      CURSRC = -9999
C
  999 RETURN
      END
      SUBROUTINE PRNTBL (UVFILE, BSTAB, OUTPRT, IRET)
C-----------------------------------------------------------------------
C   Print the contents of BSTAB on OUTPRT.
C
C   Inputs:
C      UVFILE    C*(*)    UVDATA object used to access data file
C      BSTAB     C*(*)    TABLE object used to access BS table
C      OUTPRT    C*(*)    PRINTER object used for report
C
C   Output:
C      IRET      I        Error code (0 implies no errors detected)
C
C   Preconditions:
C      UVFILE is initialized and closed
C      BSTAB is open and in time order
C      OUTPRT is open
C
C   Postconditions
C      IRET .EQ. 0 implies that
C         the BS table has been printed or the user has aborted the
C         printing
C-----------------------------------------------------------------------
      CHARACTER UVFILE*(*), BSTAB*(*), OUTPRT*(*)
      INTEGER   IRET
C
C   Local variables
C
C   SUBARR    Current subarray number
C   CATBLK    UV file header
C   CNO       UV file catalogue number
C   DISK      UV file disk number
C   MSGSAV    Saved minimum message level
C   TYPE      AIPS attribute type
C   DIM       AIPS attribute dimensions
C   CDUMMY    Placeholder
C   ANBUFF    Buffer for GETANT
C
      INTEGER   SUBARR, CATBLK(256), CNO, DISK, MSGSAV, TYPE, DIM(3),
     *   ANBUFF(512)
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSPRT.INC'
C-----------------------------------------------------------------------
      CALL PRNHDR (UVFILE, BSTAB, OUTPRT, IRET)
      IF (IRET .LT. 0) THEN
C                                       User terminated printout
         IRET = 0
         GO TO 999
      ELSE IF (IRET .GT. 0) THEN
         GO TO 999
      END IF
C
C     Get physical file information for calls to GETANT:
C
      CALL OUVCGT (UVFILE, CATBLK, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL FNAGET (UVFILE, 'CNO', TYPE, DIM, CNO, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL FNAGET (UVFILE, 'DISK', TYPE, DIM, DISK, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Process each subarray.  If GETANT fails then the subarray is
C     assumed not to exist.  Since this is not regarded as an error,
C     messages are suppressed when GETANT is called.  Note that
C     SETUP protects against the case where there are no AN tables
C     present.
C
      DO 10 SUBARR = 1, MAXSUB
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL GETANT (DISK, CNO, SUBARR, CATBLK, ANBUFF, IRET)
         MSGSUP = MSGSAV
         IF (IRET .EQ. 0) THEN
            CALL PRNSUB (UVFILE, BSTAB, OUTPRT, SUBARR, IRET)
            IF (IRET .LT. 0) THEN
               IRET = 0
               GO TO 999
            ELSE IF (IRET .GT. 0) THEN
               GO TO 999
            END IF
         ELSE
            IRET = 0
         END IF
   10 CONTINUE
C
  999 RETURN
      END
      SUBROUTINE PRNHDR (UVFILE, BSTAB, OUTPRT, IRET)
C-----------------------------------------------------------------------
C   Print a header describing BSTAB.
C
C   Inputs
C      UVFILE    C*(*)    UVDATA object used to access uv data file
C      BSTAB     C*(*)    TABLE object used to access BS table
C      OUTPRT    C*(*)    PRINTER object used for report
C
C   Output
C      IRET      I        Error code (0 implies no errors detected;
C                           < 0 implies user requested termination)
C
C   Preconditions:
C      UVFILE is initialized and closed
C      BSTAB is open
C      OUTPRT is open
C
C-----------------------------------------------------------------------
      CHARACTER UVFILE*(*), BSTAB*(*), OUTPRT*(*)
      INTEGER   IRET
C
C   Local variables
C
C   UVNAME    UV file name
C   UVCLAS    UV file class
C   UVSEQ     UV file sequence number
C   UVDISK    UV file disk number
C   BSVER     BS table version number
C   LINE      line buffer
C   QUIT      does user wish to terminate program?
C   TYPE      AIPS attribute type code
C   DIM       AIPS attribute dimension
C   CDUMMY    placeholder
C   NDUMMY    placeholder
C
      CHARACTER UVNAME*12, UVCLAS*6, LINE*132, CDUMMY
      INTEGER   UVSEQ, UVDISK, BSVER, TYPE, DIM(3), NDUMMY
      LOGICAL   QUIT
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSPRT.INC'
C-----------------------------------------------------------------------
      QUIT = .FALSE.
      CALL TABGET (BSTAB, 'VER', TYPE, DIM, BSVER, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL FNAGET (UVFILE, 'NAME', TYPE, DIM, NDUMMY, UVNAME, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL FNAGET (UVFILE, 'CLASS', TYPE, DIM, NDUMMY, UVCLAS, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL FNAGET (UVFILE, 'IMSEQ', TYPE, DIM, UVSEQ, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL FNAGET (UVFILE, 'DISK', TYPE, DIM, UVDISK, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
      WRITE (LINE, 1000) BSVER
      CALL PRTWRI (OUTPRT, LINE, QUIT, IRET)
      IF (IRET .NE. 0) GO TO 999
      IF (QUIT) THEN
         IRET = -1
         GO TO 999
      END IF
      WRITE (LINE, 1001) UVNAME(1:ITRIM(UVNAME)),
     *   UVCLAS(1:ITRIM(UVCLAS)), UVSEQ, UVDISK
      CALL PRTWRI (OUTPRT, LINE, QUIT, IRET)
      IF (IRET .NE. 0) GO TO 999
      IF (QUIT) THEN
         IRET = -1
         GO TO 999
      END IF
C
      WRITE (LINE, 1002) MODE
      CALL PRTWRI (OUTPRT, LINE, QUIT, IRET)
      IF (IRET .NE. 0) GO TO 999
      IF (QUIT) THEN
         IRET = -1
         GO TO 999
      END IF
      WRITE (LINE, 1003) NUMIF
      CALL PRTWRI (OUTPRT, LINE, QUIT, IRET)
      IF (IRET .NE. 0) GO TO 999
      IF (QUIT) THEN
         IRET = -1
         GO TO 999
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Listing for BS table version number ', I4)
 1001 FORMAT ('from file ', A, '.', A, '.', I3.3, ' on disk ', I2,
     *   '.')
 1002 FORMAT ('Baseline fringe solutions generated in mode ''', A4,
     *   '''.')
 1003 FORMAT ('Table contains solutions for ', I4, ' distinct IFs.')
      END
      SUBROUTINE PRNSUB (UVFILE, BSTAB, OUTPRT, SUBARR, IRET)
C-----------------------------------------------------------------------
C   Print solutions for SUBARR.
C
C   Inputs
C      UVFILE    C*(*)    UVDATA object used to access uv data file
C      BSTAB     C*(*)    TABLE object used to access BS table
C      OUTPRT    C*(*)    PRINTER object used for report
C      SUBARR    I        Subarray number
C
C   Output
C      IRET      I        Error code (0 implies no errors detected;
C                           < 0 implies user requested termination)
C
C   Preconditions:
C      UVFILE is initialized and closed
C      BSTAB is open
C      OUTPRT is open
C      1 <= SUBARR <= MAXSUB
C      information for SUBARR has been loaded into DANS.INC commons
C
C-----------------------------------------------------------------------
      CHARACTER UVFILE*(*), BSTAB*(*), OUTPRT*(*)
      INTEGER   SUBARR, IRET
C
C   Local variables
C
C   A1        First antenna index
C   A2        Second antenna index
C   BASELN    Baseline antenna indices
C             (indices refer to DANS.INC arrays)
C
      INTEGER   A1, A2, BASELN(2)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'BSPRT.INC'
C-----------------------------------------------------------------------
      DO 20 A1 = 1, NSTNS - 1
         DO 10 A2 = A1 + 1, NSTNS
            BASELN(1) = A1
            BASELN(2) = A2
            CALL PRNBAS (UVFILE, BSTAB, OUTPRT, SUBARR, BASELN, IRET)
            IF (IRET .NE. 0) GO TO 999
   10    CONTINUE
   20 CONTINUE
C
  999 RETURN
      END
      SUBROUTINE PRNBAS (UVFILE, BSTAB, OUTPRT, SUBARR, BASELN, IRET)
C-----------------------------------------------------------------------
C   Print solutions for BASELN in SUBARR.
C
C   Inputs
C      UVFILE    C*(*)    UVDATA object used to access uv data file
C      BSTAB     C*(*)    TABLE object used to access BS table
C      OUTPRT    C*(*)    PRINTER object used for report
C      SUBARR    I        Subarray number
C      BASELN    I(2)     Antenna indices in DANS.INC arrays defining
C                         a baseline.
C
C   Output
C      IRET      I        Error code (0 implies no errors detected;
C                           < 0 implies user requested termination)
C
C   Preconditions:
C      UVFILE is initialized and closed
C      BSTAB is open
C      OUTPRT is open
C      1 <= SUBARR <= MAXSUB
C      information for SUBARR has been loaded into DANS.INC commons
C      1 <= BASELN(1) < BASELN(2) <= NSTNS
C
C-----------------------------------------------------------------------
      CHARACTER UVFILE*(*), BSTAB*(*), OUTPRT*(*)
      INTEGER   SUBARR, BASELN(2), IRET
C
C   Local variables
C
C   POL       Stokes code (-8 <= POL <= -1)
C
      INTEGER   POL
C
      INCLUDE 'BSPRT.INC'
C-----------------------------------------------------------------------
C
C     Start by assuming that only RR is present.  Any other polariza-
C     tions present will be found on the first pass through the table.
C
      HAVPOL(-1) = .TRUE.
      DO 10 POL = -2, -8, -1
         HAVPOL(POL) = .FALSE.
   10 CONTINUE
C
C     Loop through the possible polarizations and print them if they
C     are present
C
      DO 20 POL = -1, -8, -1
         IF (HAVPOL(POL)) THEN
            CALL PRNPOL (UVFILE, BSTAB, OUTPRT, SUBARR, BASELN, POL,
     *         IRET)
            IF (IRET .NE. 0) GO TO 999
         END IF
   20 CONTINUE
C
  999 RETURN
      END
      SUBROUTINE PRNPOL (UVFILE, BSTAB, OUTPRT, SUBARR, BASELN, POL,
     *         IRET)
C-----------------------------------------------------------------------
C   Print results for Stokes code POL on baseline BASELN from SUBARR.
C
C   Inputs
C      UVFILE    C*(*)    UVDATA object used to access uv data file
C      BSTAB     C*(*)    TABLE object used to access BS table
C      OUTPRT    C*(*)    PRINTER object used for report
C      SUBARR    I        Subarray number
C      BASELN    I(2)     Antenna indices in DANS.INC arrays defining
C                         a baseline.
C      POL       I        Stokes code
C
C   Output
C      IRET      I        Error code (0 implies no errors detected;
C                           < 0 implies user requested termination)
C
C   Preconditions:
C      UVFILE is initialized and closed
C      BSTAB is open
C      OUTPRT is open
C      1 <= SUBARR <= MAXSUB
C      information for SUBARR has been loaded into DANS.INC commons
C      1 <= BASELN(1) < BASELN(2) <= NSTNS
C      -8 <= POL <= -1
C
C-----------------------------------------------------------------------
      CHARACTER UVFILE*(*), BSTAB*(*), OUTPRT*(*)
      INTEGER   SUBARR, BASELN(2), POL, IRET
C
C     Solution mode
C
C     MODES     Solution mode names
C     MODENO    Solution mode number
C
      CHARACTER MODES(4)*4
      INTEGER   MODENO
C
C     Width calculation
C
C     COLS      Width of each field in each mode
C     CURFLD    Current field number
C     WIDTH     Current cumulative width
C     NACROS    Width of print-out
C     FLD1      First varying field in current pass
C     NXTFLD    First varying field in next pass
C     DOCRT     Value of DOCRT adverbs
C
      INTEGER   COLS(11, 4), CURFLD, WIDTH, NACROS, FLD1, NXTFLD, DOCRT
C
C     Titles
C
C     TITLE1    First row of page title
C     TITLE2    Second row of page title
C     TITLES    Column titles
C
      CHARACTER TITLE1*80, TITLE2*132, TITLES(11)*24
C
C     Other
C
C     TYPE      AIPS attribute type
C     DIM       AIPS attribute dimensions
C     CDUMMY    Placeholder
C     NDUMMY    Placeholder
C     POLNAM    Polarization names
C
      INTEGER   TYPE, DIM(3), NDUMMY
      CHARACTER POLNAM(-8:-1)*2, CDUMMY
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSPRT.INC'
C
      DATA MODES /'INDE', 'VLBA', 'MK3 ', 'RATE'/
      DATA COLS /16, 12, 18, 6, 14, 14,  0, 23, 23, 23, 17,
     *           16, 12, 18, 0, 14, 14,  0, 23, 23, 23, 17,
     *           16, 12, 18, 0, 14, 14, 23, 23, 23, 23, 17,
     *           16, 12, 18, 0, 14, 14,  0,  0, 23, 23, 17/
      DATA TITLES /'      Time              ',
     *             '  Interval              ',
     *             '     Source             ',
     *             '  IF                    ',
     *             'Vector Amp/Jy           ',
     *             'Scalar Amp/Jy           ',
     *             '  Multiband Delay/ns    ',
     *             ' Single-band Delay/ns   ',
     *             '       Rate/mHz         ',
     *             '  Acceleration/uHz**2   ',
     *             '   Phase/deg            '/
      DATA POLNAM /'YX', 'XY', 'YY', 'XX', 'LR', 'RL', 'LL', 'RR'/
C-----------------------------------------------------------------------
C
C     Get the number of columns in the printer output from the printer
C     object
C
      CALL PRTGET (OUTPRT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
      NACROS = 72
      IF (DOCRT .LE. 0) NACROS = NCHPRT
      IF ((DOCRT .GT. 72) .AND. (DOCRT .LE. 132)) NACROS = DOCRT
      IF (DOCRT .GT. 132) NACROS = 132
C
C     Set the main title
C
      IF (TELNO(BASELN(1)) .LT. TELNO(BASELN(2))) THEN
         WRITE (TITLE1, 1000)
     *      STNNAM(BASELN(1))(1:ITRIM(STNNAM(BASELN(1)))),
     *      STNNAM(BASELN(2))(1:ITRIM(STNNAM(BASELN(2)))),
     *      SUBARR, POLNAM(POL)
      ELSE
         WRITE (TITLE1, 1000)
     *      STNNAM(BASELN(2))(1:ITRIM(STNNAM(BASELN(2)))),
     *      STNNAM(BASELN(1))(1:ITRIM(STNNAM(BASELN(1)))),
     *      SUBARR, POLNAM(POL)
      END IF
      TYPE = OOACAR
      DIM(1) = ITRIM(TITLE1)
      DIM(2) = 1
      DIM(3) = 0
      CALL PRTPUT (OUTPRT, 'TITLE1', TYPE, DIM, NDUMMY, TITLE1, IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Find the mode number:
C
      MODENO = 1
   10 IF (MODENO .LE. 4) THEN
         IF (MODES(MODENO) .NE. MODE) THEN
            MODENO = MODENO + 1
            GO TO 10
         END IF
      END IF
      IF (MODENO .GT. 4) THEN
         WRITE (MSGTXT, 1010) MODE
         CALL MSGWRT (8)
         IRET = 999
         GO TO 999
      END IF
C
C     Make as many passes through the file as is necessary to print
C     all of the required data:
C
      NXTFLD = 5
   20 IF (NXTFLD .LE. 11) THEN
C
C        Find out how many fields can be printed in the current pass
C        and set the column headers in the second line of the page
C        header.  The first 4 fields (time, time interval, source
C        and IF) will appear in every pass.
C
         WIDTH = 0
         TITLE2 = ' '
         DO 30 CURFLD = 1, 4
            TITLE2(WIDTH+1:WIDTH+COLS(CURFLD, MODENO)) =
     *         TITLES(CURFLD)(1:COLS(CURFLD, MODENO))
            WIDTH = WIDTH + COLS(CURFLD, MODENO)
   30    CONTINUE
         FLD1 = NXTFLD
   40    IF (NXTFLD .LE. 11) THEN
            IF (WIDTH + COLS(NXTFLD, MODENO) .LE. NACROS) THEN
               TITLE2(WIDTH+1:WIDTH+COLS(NXTFLD, MODENO)) =
     *            TITLES(NXTFLD)(1:COLS(NXTFLD, MODENO))
               WIDTH = WIDTH + COLS(NXTFLD, MODENO)
               NXTFLD = NXTFLD + 1
               GO TO 40
            END IF
         END IF
C
C        Set the second line of the page title and force the next line
C        to start a new page:
C
         TYPE = OOACAR
         DIM(1) = ITRIM (TITLE2)
         DIM(2) = 1
         DIM(3) = 0
         CALL PRTPUT (OUTPRT, 'TITLE2', TYPE, DIM, NDUMMY, TITLE2, IRET)
         IF (IRET .NE. 0) GO TO 999
         CALL PRTNUP (OUTPRT, IRET)
         IF (IRET .NE. 0) GO TO 999
C
C        Traverse the table:
C
         CALL PRNPAS (UVFILE, BSTAB, OUTPRT, SUBARR, BASELN, POL, FLD1,
     *      NXTFLD - 1, COLS, MODENO, IRET)
         IF (IRET .NE. 0) GO TO 999
C
         GO TO 20
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Baseline ', A, '-', A, ' from subarray ', I3,
     *   ' polarization ', A2)
 1010 FORMAT ('MODE ''', A4, ''' IS NOT RECOGNIZED - ABORTING')
      END
      SUBROUTINE PRNPAS (UVFILE, BSTAB, OUTPRT, SUBARR, BASELN, POL,
     *   START, FINISH, COLS, MODENO, IRET)
C-----------------------------------------------------------------------
C   Print out the data for the varying fields START through FINISH
C   together with the 4 fixed fields for BASELN, SUBARR and POL.
C
C   Inputs
C      UVFILE    C*(*)    UVDATA object used to access uv data file
C      BSTAB     C*(*)    TABLE object used to access BS table
C      OUTPRT    C*(*)    PRINTER object used for report
C      SUBARR    I        Subarray number
C      BASELN    I(2)     Antenna indices in DANS.INC arrays defining
C                         a baseline.
C      POL       I        Stokes code
C      START     I        First varying field to print
C      FINISH    I        Last varying field to print
C      COLS      I(11, 4) Width of each field in each mode
C      MODENO    I        Mode number
C
C   Output
C      IRET      I        Error code (0 implies no errors detected;
C                           < 0 implies user requested termination)
C
C   Preconditions:
C      UVFILE is initialized and closed
C      BSTAB is open
C      OUTPRT is open
C      1 <= SUBARR <= MAXSUB
C      information for SUBARR has been loaded into DANS.INC commons
C      1 <= BASELN(1) < BASELN(2) <= NSTNS
C      -8 <= POL <= -1
C      5 <= START <= FINISH <= 11
C      COLS(1:11, 1:4) >= 0
C      1 <= MODENO <= 4
C
C-----------------------------------------------------------------------
      CHARACTER UVFILE*(*), BSTAB*(*), OUTPRT*(*)
      INTEGER   SUBARR, BASELN(2), POL, START, FINISH, COLS(11, 4),
     *   MODENO, IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C   BS table
C
C   ROW       Current row number
C   NROWS     Number of rows in table
C   TIME      Timestamp (days)
C   INTERV    Time interval (days)
C   BL        Baseline
C   SUB       Subarray
C   STOKES    Stokes code
C   SRCID     Source ID
C   VAMP      Vector amplitude sum (Jy)
C   SAMP      Scalar amplitude sum (Jy)
C   RMBD      Residual multiband delay (s)
C   MBDERR    Residual multiband delay error (s)
C   MBDAMB    Residual multiband delay ambiguity (s)
C   RSBD      Residual single-band delay (s)
C   SBDERR    Residual single-band delay error (s)
C   SBDAMB    Residual single-band delay ambiguity (s)
C   RRATE     Residual rate (Hz)
C   RTERR     Residual rate error (Hz)
C   RTAMB     Residual-rate ambiguity (Hz)
C   RACCEL    Residual acceleration (Hz**2)
C   ACCERR    Residual acceleration error (Hz**2)
C   RPHASE    Residual phase (deg)
C   PHSERR    Residual-phase error (deg)
C
      INTEGER   ROW, NROWS, BL(2), SUB, STOKES, SRCID
      DOUBLE PRECISION TIME
      REAL INTERV, VAMP(MAXIF), SAMP(MAXIF), RMBD, MBDERR, MBDAMB,
     *   RSBD(MAXIF), SBDERR(MAXIF), SBDAMB(MAXIF), RRATE(MAXIF),
     *   RTERR(MAXIF), RTAMB(MAXIF), RACCEL(MAXIF), ACCERR(MAXIF),
     *   RPHASE(MAXIF), PHSERR(MAXIF)
C
C   uv file details for GETSOU
C
C   CATBLK    Header record
C   CNO       Catalogue number
C   DISK      Disk number
C
      INTEGER   CATBLK(256), CNO, DISK
C
C   Formatting and print output
C
C   COL       Current output column
C   LINE      Output line
C   FMTBUF    Formatting buffer
C   CURFLD    Current field
C   QUIT      Does the user want to finish?
C
      INTEGER   COL, CURFLD
      CHARACTER LINE*132, FMTBUF*24
      LOGICAL   QUIT
C
C   Other local variables
C
C   IFNUM     Current IF number
C   MSGSAV    Saved message suppression level
C   DHMS      Split time (day, hour. min, sec)
C   TYPE      AIPS attribute type code
C   DIM       AIPS attribute dimensions
C   CDUMMY    Placeholder
C
      INTEGER   IFNUM, MSGSAV, TYPE, DIM(3)
      REAL      DHMS(4)
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'BSPRT.INC'
C-----------------------------------------------------------------------
      QUIT = .FALSE.
C
C     Extract file information for future calls to GETSOU
C
      CALL OUVCGT (UVFILE, CATBLK, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL OUVGET (UVFILE, 'CNO', TYPE, DIM, CNO, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL OUVGET (UVFILE, 'DISK', TYPE, DIM, DISK, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Find the number of rows in the table
C
      CALL TABGET (BSTAB, 'NROW', TYPE, DIM, NROWS, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Scan the table
C
      ROW = 1
   10 IF (ROW .LE. NROWS) THEN
         CALL OTABBS (BSTAB, 'READ', ROW, NUMIF, TIME, INTERV, BL, SUB,
     *      STOKES, SRCID, VAMP, SAMP, RMBD, MBDERR, MBDAMB, RSBD,
     *      SBDERR, SBDAMB, RRATE, RTERR, RTAMB, RACCEL, ACCERR,
     *      RPHASE ,PHSERR, IRET)
         IF (IRET .GT. 0) GO TO 999
         IF (IRET .EQ. 0) THEN
C                                       Unflagged
            HAVPOL(STOKES) = .TRUE.
            IF ((((BL(1) .EQ. BASELN(1)) .AND. (BL(2) .EQ. BASELN(2)))
     *         .OR. ((BL(1) .EQ. BASELN(2))
     *         .AND. (BL(2) .EQ. BASELN(1)))) .AND. (SUB .EQ. SUBARR)
     *         .AND. (STOKES .EQ. POL)) THEN
C
C              Read in new source information if the source has changed.
C              Suppress error messages since errors are non-fatal
C
               IF (SRCID .NE. CURSRC) THEN
                  MSGSAV = MSGSUP
                  MSGSUP = 3200
                  CALL GETSOU (SRCID, DISK, CNO, CATBLK, 25, IRET)
                  MSGSUP = MSGSAV
                  IF (IRET .NE. 0) THEN
                     IRET = 0
                     SNAME = 'Unknown source'
                  END IF
                  CURSRC = SRCID
               END IF
C
C              Process each IF
C
               DO 50 IFNUM = 1, NUMIF
                  LINE = ' '
                  COL = 0
                  IF (IFNUM .EQ. 1) THEN
                     CALL TSPLIT (REAL (TIME), DHMS)
                     WRITE (FMTBUF, 1010) INT(DHMS(1)), INT(DHMS(2)),
     *                  INT(DHMS(3)), DHMS(4)
                     LINE(COL+1:COL+COLS(1, MODENO)) =
     *                  FMTBUF(1:COLS(1, MODENO))
                  END IF
                  COL = COL + COLS(1, MODENO)
                  IF (IFNUM .EQ. 1) THEN
                     CALL TSPLIT (INTERV, DHMS)
                     WRITE (FMTBUF, 1011) INT(DHMS(2)), INT(DHMS(3)),
     *                  DHMS(4)
                     LINE(COL+1:COL+COLS(2, MODENO)) =
     *                  FMTBUF(1:COLS(2, MODENO))
                  END IF
                  COL = COL + COLS(2, MODENO)
                  IF (IFNUM .EQ. 1) THEN
                     LINE(COL+1:COL + COLS(3, MODENO)) =
     *                  SNAME(1:COLS(3, MODENO))
                  END IF
                  COL = COL + COLS(3, MODENO)
                  WRITE (FMTBUF, 1012) IFNUM
                  LINE(COL+1:COL+COLS(4, MODENO)) =
     *               FMTBUF(1:COLS(4, MODENO))
                  COL = COL + COLS(4, MODENO)
                  DO 40 CURFLD = START, FINISH
                     FMTBUF = ' '
                     IF (COLS(CURFLD, MODENO) .GE. 0) THEN
                        IF (CURFLD .EQ. 5) THEN
                           IF (VAMP(IFNUM) .NE. FBLANK) THEN
                              WRITE (FMTBUF, 1013) VAMP(IFNUM)
                           ELSE
                              WRITE (FMTBUF, 1014)
                           END IF
                        ELSE IF (CURFLD .EQ. 6) THEN
                           IF (SAMP(IFNUM) .NE. FBLANK) THEN
                              WRITE (FMTBUF, 1013) SAMP(IFNUM)
                           ELSE
                              WRITE (FMTBUF, 1014)
                           END IF
                        ELSE IF ((CURFLD .EQ. 7) .AND. (IFNUM .EQ. 1))
     *                        THEN
                           IF ((RMBD .NE. FBLANK) .AND.
     *                        (MBDERR .NE. FBLANK)) THEN
                              WRITE (FMTBUF, 1015) 1.0E9 * RMBD,
     *                           1.0E9 * MBDERR
                           ELSE
                              WRITE (FMTBUF, 1016)
                           END IF
                        ELSE IF (CURFLD .EQ. 8) THEN
                           IF ((RSBD(IFNUM) .NE. FBLANK) .AND.
     *                        (SBDERR(IFNUM) .NE. FBLANK)) THEN
                              WRITE (FMTBUF, 1015) 1.0E9 * RSBD(IFNUM),
     *                           1.0E9 * SBDERR(IFNUM)
                           ELSE
                              WRITE (FMTBUF, 1016)
                           END IF
                        ELSE IF (CURFLD .EQ. 9) THEN
                           IF ((RRATE(IFNUM) .NE. FBLANK) .AND.
     *                        (RTERR(IFNUM) .NE. FBLANK)) THEN
                              WRITE (FMTBUF, 1015) 1.0E3 * RRATE(IFNUM),
     *                           1.0E3 * RTERR(IFNUM)
                           ELSE
                              WRITE (FMTBUF, 1016)
                           END IF
                        ELSE IF (CURFLD .EQ. 10) THEN
                           IF ((RACCEL(IFNUM) .NE. FBLANK) .AND.
     *                        (ACCERR(IFNUM) .NE. FBLANK)) THEN
                              WRITE (FMTBUF, 1015)
     *                           1.0E6 * RACCEL(IFNUM),
     *                           1.0E6 * ACCERR(IFNUM)
                           ELSE
                              WRITE (FMTBUF, 1016)
                           END IF
                        ELSE IF (CURFLD .EQ. 11) THEN
                           IF ((RPHASE(IFNUM) .NE. FBLANK) .AND.
     *                        (PHSERR(IFNUM) .NE. FBLANK)) THEN
                              WRITE (FMTBUF, 1017) RPHASE(IFNUM),
     *                           PHSERR(IFNUM)
                           ELSE
                              WRITE (FMTBUF, 1018)
                           END IF
                        END IF
                     END IF
                     LINE(COL+1:COL+COLS(CURFLD, MODENO)) =
     *                  FMTBUF(1: COLS(CURFLD, MODENO))
                     COL = COL + COLS(CURFLD, MODENO)
   40             CONTINUE
                  CALL PRTWRI (OUTPRT, LINE, QUIT, IRET)
                  IF (IRET .NE. 0) GO TO 999
                  IF (QUIT) THEN
                     IRET = -1
                     GO TO 999
                  END IF
   50          CONTINUE
            END IF
         END IF
         GO TO 10
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (I3.3, '/', I2.2, ':', I2.2, ':', F4.1, '  ')
 1011 FORMAT (I2.2, ':', I2.2, ':', F4.1, '  ')
 1012 FORMAT (I4, '  ')
 1013 FORMAT (E12.6, '  ')
 1014 FORMAT ('    INDE    ')
 1015 FORMAT (F8.3, ' +/- ', F8.3, '  ')
 1016 FORMAT ('         INDE          ')
 1017 FORMAT (F6.1, ' +/- ', F6.1)
 1018 FORMAT ('      INDE       ')
      END
      SUBROUTINE TSPLIT(DAYS, TIME)
C-----------------------------------------------------------------------
C   Split a time in days into its component parts.
C
C   Input:
C      DAYS    R       Time in days
C
C   Output:
C      TIME    R(4)    Time in days, hours, minutes and seconds
C-----------------------------------------------------------------------
      REAL   DAYS, TIME(4)
C
      REAL   D
C-----------------------------------------------------------------------
      TIME(1) = INT (DAYS)
      D = 24.0 * (DAYS - TIME(1))
      TIME(2) = INT (D)
      D = 60.0 * (D - TIME(2))
      TIME(3) = INT (D)
      TIME(4) = 60.0 * (D - TIME(3))
      END

