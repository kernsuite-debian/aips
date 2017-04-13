LOCAL INCLUDE 'FXMAPS.INC'
C
C   Information relating input IFs to output IFs and polarizations.
C
C     STK1      The reference value for the output STOKES axis
C     STKDIM    The length of the output STOKES axis
C     NSTK      The number of input IFs for which the output
C               polarization has been defined
C     STKIDX    A look-up table from input IF to output STOKES axis
C               index
C     POLSTR    A look-up table from input IF to output polarization
C               character code
C     IFDIM     The length of the output IF axis
C     IFINDX    A look-up table from input IF to output IF axis index
C
      INCLUDE 'INCS:PUVD.INC'
C
      INTEGER   STK1
      INTEGER   STKDIM
      INTEGER   NSTK
      INTEGER   STKIDX(MAXIF)
      CHARACTER POLSTR*(MAXIF)
      INTEGER   IFDIM
      INTEGER   IFINDX(MAXIF)
C
      COMMON /FXMAPS/ STK1, STKDIM, NSTK, STKIDX, IFDIM, IFINDX
      SAVE   /FXMAPS/
      COMMON /FXMAPC/ POLSTR
      SAVE   /FXMAPC/
C
LOCAL END
      PROGRAM FXPOL
C-----------------------------------------------------------------------
C! Corrects polarizations for VLBA dual-polarization data
C# UV-util VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 2000, 2005, 2009, 2011-2012, 2015
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
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Create a new file containing the same data as the input file but
C   with polarizations and IFs reassigned according to a function
C   specified by the user.
C
C   If the user has specified a valid mapping from IFs to polarizations
C   using BANDPOL, it is possible to deterimine a full mapping from
C   input IFs to output IFs and polarizations, and it is possible to
C   transform the data and tables belonging to the input file and write
C   them to the specified output file then create a new output file,
C   transform the input data and tables to conform to the new
C   polarization structure, write the transfromed data and tables to the
C   newly created file and signal successful completion to POPS.
C
C   Otherwise issue one or more error messages and signal failure to
C   POPS while leaving the output file in any state.
C-----------------------------------------------------------------------
C
C   Local variables:
C
C     TASKNM      Task name (constant)
C
C     NPARM       Number of input adverbs (constant)
C     AVNAME      Names of the input adverbs
C     AVTYPE      Type codes for the input adverbs
C     AVDIM       Dimensions of the value arrays for the input adverbs
C
C     INPUTS      Name of INPUTS object that holds adverb values
C     INFILE      Name of UVDATA object that manages the input file
C     OUTFIL      Name of UVDATA object that manages the output file
C
C     IRET        Task return status - 0 for success, non-zero for
C                 failure
C     IRET1       Return code from OUVCLO
C
C     SCRTCH      Scratch buffer
C
      CHARACTER TASKNM*6
      PARAMETER (TASKNM = 'FXPOL ')
C
      INTEGER   NPARM
      PARAMETER (NPARM = 10)
      CHARACTER AVNAME(NPARM)*8
      INTEGER   AVTYPE(NPARM)
      INTEGER   AVDIM(2, NPARM)
C
      CHARACTER INPUTS*6
      PARAMETER (INPUTS = 'Inputs')
      CHARACTER INFILE*10
      PARAMETER (INFILE = 'Input file')
      CHARACTER OUTFIL*11
      PARAMETER (OUTFIL = 'Output file')
C
      INTEGER   IRET
      INTEGER   IRET1
C
      INTEGER   SCRTCH(256)
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C
      DATA AVNAME / 'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *              'OUTNAME ', 'OUTCLASS', 'OUTSEQ  ', 'OUTDISK ',
     *              'BANDPOL ', 'FQTOL   ' /
      DATA AVTYPE / OOACAR,     OOACAR,     OOAINT,     OOAINT,
     *              OOACAR,     OOACAR,     OOAINT,     OOAINT,
     *              OOACAR,     OOARE      /
      DATA AVDIM  / 12, 1,      6, 1,       1, 1,       1, 1,
     *              12, 1,      6, 1,       1, 1,       1, 1,
     *               8, 1,      1, 1       /
C-----------------------------------------------------------------------
C
C     If the task can be initialized, the input adverbs can be read, the
C     BANDPOL adverb has valid syntax, and the designated input file can
C     be opened for reading then replace out-of-range adverb values with
C     the appropriate default values, store the adverb values in INPUTS,
C     associate the designated input file with INFILE, open the input
C     file for reading and set IRET to zero.
C     Otherwise issue one or more fatal error messages and set IRET to
C     a non-zero value, leaving INPUTS and INFILE in any state.
C
      CALL FXINP (INPUTS, INFILE, TASKNM, NPARM, AVNAME, AVTYPE, AVDIM,
     *            IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If it is possible to determine a full mapping from input IFs
C        to output IFs and polarizations, the designated output file can
C        be created, and it is possible to transform the contents of the
C        input file and write them to the output file then create the
C        output file and transfer the transformed data and tables to it
C        and set IRET to 0, removing any status flags from the output
C        file and leaving it closed.
C        Otherwise, issue one or more error messages and set IRET to a
C        non-zero value, leaving the output file in any state.
C
         CALL FXOUT (INPUTS, INFILE, OUTFIL, IRET)
C
C        If it is possible to close the input file then close it and
C        set IRET1 to zero otherwise set IRET1 to a on-zero value and
C        leave the input file in any state:
C
         CALL OUVCLO (INFILE, IRET1)
C
         IF (IRET1 .EQ. 0) THEN
C
C           The input file has been closed. Note that both files must be
C           closed before history can be copied.
C
            IF (IRET .EQ. 0) THEN
C
C              The data were transformed and successfully copied to the
C              output file.
C
C              If the history records can be copied from the input file
C              to the output file and the output file history can be
C              updated then copy the history from the input file to the
C              output file, add information about this run of FXPOL, and
C              set IRET to zero.
C              Otherwise issue one or more error messages and set IRET
C              to a non-zero value.
C
               CALL FXHIST (INPUTS, INFILE, OUTFIL, NPARM, AVNAME, IRET)
C
            END IF
         ELSE
C
C           The input file could not be closed.
C
            WRITE (MSGTXT, 9000) IRET1
            CALL MSGWRT (9)
            IRET = IRET1
         END IF
C
      END IF
C
C     Clean up and return the program status to POPS:
C
      CALL DIE (IRET, SCRTCH)
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPOL: UNABLE TO CLOSE INPUT FILE (ERROR ', I4, ')')
      END
      SUBROUTINE FXINP (INPUTS, INFILE, TASKNM, NPARM, AVNAME, AVTYPE,
     *                  AVDIM, IRET)
C-----------------------------------------------------------------------
C   Initialize the task and open the input file.
C
C   Inputs:
C      INPUTS  C*(*)       Name of INPUTS object to hold adverbs
C      INFILE  C*(*)       Name of UVDATA object used to manage input
C                          file
C      TASKNM  C*6         Task name
C      NPARM   I           The number of inputs for FXPOL
C      AVNAME  C(NPARM)*8  Names of task adverbs
C      AVTYPE  I(NPARM)    Type codes for task adverbs
C      AVDIM   I(2, NPARM) Array dimensions for task adverbs
C
C   Output:
C      IRET    I           Status indicator - 0 on success, non-zero on
C                          failure
C
C   Preconditions:
C     Neither INPUTS nor INFILE is blank
C     INPUTS is not the same as INFILE
C     TASKNM is 'FXPOL '
C     AVNAME contains 'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BANDPOL'
C        and 'FQTOL'.
C     The entries in AVTYPE corresponding to 'INNAME', 'INCLASS', and
C        'BANDPOL' desclare these adverbs as character strings.
C     The entries in AVTYPE corresponding to 'INSEQ' and 'INDISK'
C        declare these adverbs as integer.
C     The entry in AVTYPE corresponding to 'FQTOL' declares that adverb
C        to be a floating-point number.
C     AVDIM declares 'INNAME' to be a scalar string of 12 characters.
C     AVDIM declares 'INCLASS' to be a scalar string of 6 characters.
C     AVDIM declares 'BANDPOL' to be a scaler string of 8 characters.
C     AVDIM declares 'INSEQ', 'INDISK', and 'FQTOL' to be scalar
C     numbers.
C
C   Postconditions:
C     If IRET is zero then
C        - INPUTS has been initialized and contains the values of the
C          adverbs listed in AVNAME with any out-of-range values
C          replaced by suitable defaults
C        - INFILE has been initialized to refer to the input file
C          specified by 'INNAME', 'INCLASS', 'INSEQ', and 'INDISK' and
C          this file has been opened for reading.
C        - a preliminary mapping from IF numbers to output polarizations
C          has been established.
C     If IRET is not zero then
C        - a fatal error message has been issued
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*)
      CHARACTER INFILE*(*)
      CHARACTER TASKNM*6
      INTEGER   NPARM
      CHARACTER AVNAME(NPARM)*8
      INTEGER   AVTYPE(NPARM)
      INTEGER   AVDIM(2, NPARM)
      INTEGER   IRET
C
C   Local variables:
C
C     NKEY     Number of keyword values to copy from INPUTS to INFILE
C              (constant)
C     INKEY    Adverbs specifying the input file in INPUTS
C     OUTKEY   INFILE attributes to recieve the INKEY values from INPUTS
C
C     FQTOL    Frequency tolerance
C     FQTLIM   Minimum allowed frequency tolerance in Hz (constant)
C     FQTDEF   Default frequency tolerance in Hz (constant)
C
C     BNDPOL   Band polarization specification string
C     BNDDEF   Default band polarization specification string (constant)
C
C     TYPE     Attribute type code
C     DIM      Attribute dimensions
C     CDUMMY   Dummy character argument
C     NDUMMY   Dummy numeric argument
C
      INTEGER   NKEY
      PARAMETER (NKEY = 4)
      CHARACTER INKEY(NKEY)*8
      CHARACTER OUTKEY(NKEY)*15
C
      REAL      FQTOL
      REAL      FQTLIM
      PARAMETER (FQTLIM = 1.0)
      REAL      FQTDEF
      PARAMETER (FQTDEF = 10.0)
C
      CHARACTER BNDPOL*8
      CHARACTER BNDDEF*8
      PARAMETER (BNDDEF = '*(RL)   ')
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
      REAL      NDUMMY
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA INKEY  / 'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ' /
      DATA OUTKEY / 'FILE_NAME.NAME ', 'FILE_NAME.CLASS',
     *              'FILE_NAME.IMSEQ', 'FILE_NAME.DISK ' /
C-----------------------------------------------------------------------
C
C     Attempt to initialize INPUTS and read the input adverbs:
C
      CALL AV2INP (TASKNM, NPARM, AVNAME, AVTYPE, AVDIM, INPUTS, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        INPUTS was initialized and the input adverbs were read
C        successfully.
C
C        If the value of the FQTOL adverb is less than FQLIM then
C        replace it with the default value FQTDEF:
C
         CALL INGET (INPUTS, 'FQTOL', TYPE, DIM, FQTOL, CDUMMY, IRET)
         IF (FQTOL .LT. FQTLIM) THEN
            FQTOL = FQTDEF
            CALL INPUTT (INPUTS, 'FQTOL', TYPE, DIM, FQTOL, CDUMMY,
     *                   IRET)
         END IF
C
C        If the value of the BANDPOL adverb is blank then replace it
C        with the default string BNDDEF:
C
         CALL INGET (INPUTS, 'BANDPOL', TYPE, DIM, NDUMMY, BNDPOL, IRET)
         IF (BNDPOL .EQ. '        ') THEN
            BNDPOL = BNDDEF
            CALL INPUTT (INPUTS, 'BANDPOL', TYPE, DIM, NDUMMY, BNDPOL,
     *                   IRET)
         END IF
C
C        Attempt to set up the preliminary mapping from IF numbers to
C        polarizations:
C
         CALL FXPMAP (BNDPOL, IRET)
         IF (IRET .EQ. 0) THEN
C
C           The preliminary polarization mapping has been established.
C
C           Attempt to create the INFILE object:
C
            CALL OUVCRE (INFILE, IRET)
            IF (IRET .EQ. 0) THEN
C
C              The INFILE object was created successfully.
C
C              Copy the values of the INNAME, INCLASS, INSEQ, and
C              INDISK adverbs to the name fields of IN2OBJ and set
C              IRET to zero:
C
               CALL IN2OBJ (INPUTS, NKEY, INKEY, OUTKEY, INFILE, IRET)
C
C              Attempt to open the input file for "raw" access:
C
               CALL OUVOPN (INFILE, 'RRAW', IRET)
               IF (IRET .NE. 0) THEN
C
C                 Failed to open the file.
C
                  WRITE (MSGTXT, 9000) IRET
                  CALL MSGWRT (9)
               END IF
C
            ELSE
C
C              Failed to create the UVDATA object INFILE.
C
               WRITE (MSGTXT, 9001) IRET
               CALL MSGWRT (9)
            END IF
         END IF
      ELSE
C
C        Failed to initialize INPUTS object, read adverbs or initialize
C        AIPS run-time support.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXINP: UNABLE TO OPEN INPUT FILE (ERROR ', I4, ')')
 9001 FORMAT ('FXINP: UNABLE TO CREATE INPUT UVDATA OBJECT (ERROR ', I4,
     *        ')')
 9002 FORMAT ('FXINP: UNABLE TO INITIALIZE TASK (ERROR ', I4, ')')
      END
      SUBROUTINE FXOUT (INPUTS, INFILE, OUTFIL, IRET)
C-----------------------------------------------------------------------
C   Create output file and transfer data and tables to it.
C
C   Intended function:
C
C     [ INPUTS and INFILE are initialized and INFILE is open for reading
C           [ INFILE has a STOKES axis with only one value
C             and INFILE has an IF axis and an FQ table
C             and a polarization mapping has been defined for each IF
C                 in INFILE
C             and a full polarization/IF mapping can be determined from
C                 INFILE and the polarization mapping
C             and OUTFIL can be initialized and the corresponding file
C                 created
C             and all data and necessary tables can be copied from the
C                 input file to the output file ->
C                    OUTFIL, IRET := relabeled version of INFILE, 0
C           | else ->
C                 OUTFIL, message file, IRET :=
C                    any, previous messages + fatal error message,
C                    non-zero ]
C     ]
C
C   Inputs:
C     INPUTS   C*(*)       Name of INPUTS object holding adverb values
C     INFILE   C*(*)       Name of UVDATA object used to access input
C                          file
C     OUTFIL   C*(*)       Name of UVDATA object used to access output
C                          file
C
C   Output:
C     IRET     I           Status on return - 0 if successful, non-zero
C                          otherwise
C
C   Preconditions:
C      The INPUTS and INFILE objects have been initialized.
C      OUTFIL is a unique object name.
C      The input file is open.
C      The INPUTS object contains values for 'OUTNAME', 'OUTCLASS',
C      'OUTSEQ', and 'OUTDISK'.
C      The INPUTS object contains a positive value for 'FQTOL'.
C      A mapping from input IFs to output polarizations has been
C      established.
C
C   Postconditions:
C      If IRET is zero then the output data file is a copy of the input
C      file that has been relabelled as requested by the user.
C      If IRET is not zero then at least one fata error message has been
C      issued.
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*)
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   IRET
C
C   Local variables:
C
C     FQTOL    Frequency tolerance in Hz
C
C     INNAME   Input file name
C     INCLAS   Input file class
C     INSEQ    Input file sequence number
C     OUTNAM   Output file name
C     OUTCLA   Output file class
C     OUTSEQ   Output file sequence number
C     OUTDSK   Output file disk number
C
C     CNO      Catalogue number from CATDIR
C     SCRTCH   Scratch buffer for CATDIR
C
C     IRET1    Alternate return status
C
C     MSGTMP   Temporary storage for message suppression level
C
C     TYPE     Attribute type code
C     DIM      Attribute dimensions
C     CDUMMY   Dummy character argument
C     NDUMMY   Dummy numeric argument
C
      REAL      FQTOL
C
      CHARACTER INNAME*12
      CHARACTER INCLAS*6
      INTEGER   INSEQ
      CHARACTER OUTNAM*12
      CHARACTER OUTCLA*6
      INTEGER   OUTSEQ
      INTEGER   OUTDSK
C
      INTEGER   CNO
      INTEGER   SCRTCH(256)
C
      INTEGER   IRET1
C
      INTEGER   MSGTMP
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
      INTEGER   NDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Complete the mapping from input IFs to output IFs and
C     polarizations, if possible:
C
      CALL INGET (INPUTS, 'FQTOL', TYPE, DIM, FQTOL, CDUMMY, IRET)
      FQTOL = 1000.0 * FQTOL
      CALL FXBMAP (INFILE, FQTOL, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        A full mapping has been established.
C
         CALL OUVCRE (OUTFIL, IRET)
         IF (IRET .EQ. 0) THEN
C
C           The OUTFIL object was successfully allocated.
C
C           Expand wild-card components of the output file name
C           according to the standard AIPS rules, set the name
C           attributes of the OUTFIL object, and set IRET to zero:
C
            CALL FNAGET (INFILE, 'NAME', TYPE, DIM, NDUMMY, INNAME,
     *                   IRET)
            CALL FNAGET (INFILE, 'CLASS', TYPE, DIM, NDUMMY, INCLAS,
     *                   IRET)
            CALL FNAGET (INFILE, 'IMSEQ', TYPE, DIM, INSEQ, CDUMMY,
     *                   IRET)
            CALL INGET (INPUTS, 'OUTNAME', TYPE, DIM, NDUMMY, OUTNAM,
     *                  IRET)
            CALL INGET (INPUTS, 'OUTCLASS', TYPE, DIM, NDUMMY,
     *                  OUTCLA, IRET)
            CALL INGET (INPUTS, 'OUTSEQ', TYPE, DIM, OUTSEQ, CDUMMY,
     *                  IRET)
            CALL INGET (INPUTS, 'OUTDISK', TYPE, DIM, OUTDSK, CDUMMY,
     *                  IRET)
            CALL MAKOUT (INNAME, INCLAS, INSEQ, '      ', OUTNAM,
     *                   OUTCLA, OUTSEQ)
            TYPE = OOACAR
            DIM(1) = 12
            DIM(2) = 1
            DIM(3) = 0
            CALL FNAPUT (OUTFIL, 'NAME', TYPE, DIM, NDUMMY, OUTNAM,
     *                   IRET)
            DIM(1) = 6
            CALL FNAPUT (OUTFIL, 'CLASS', TYPE, DIM, NDUMMY, OUTCLA,
     *                      IRET)
            TYPE = OOAINT
            DIM(1) = 1
            CALL FNAPUT (OUTFIL, 'IMSEQ', TYPE, DIM, OUTSEQ, CDUMMY,
     *                   IRET)
            CALL FNAPUT (OUTFIL, 'DISK', TYPE, DIM, OUTDSK, CDUMMY,
     *                   IRET)
C
C           If OUTSEQ is not zero then the output file may already exist
C           so it is necessary to check. In this case there is a small
C           risk that another task may create an output file with the
C           same name between the call to CATDIR and the OUVOPN call
C           that creates the output file. This risk is assumed to be
C           negligable.
C
            IF (OUTSEQ .NE. 0) THEN
               CNO = 0
               MSGTMP = MSGSUP
               MSGSUP = 32000
               CALL CATDIR ('SRNH', OUTDSK, CNO, OUTNAM, OUTCLA,
     *                      OUTSEQ, '  ', NLUSER, '    ', SCRTCH,
     *                      IRET1)
               MSGSUP = MSGTMP
               IF (IRET1 .EQ. 0) THEN
                  MSGTXT = 'CAN NOT OVERWRITE AN EXISTING FILE'
                  CALL MSGWRT (9)
                  IRET = 1
               ELSE IF (IRET1 .NE. 5) THEN
                  WRITE (MSGTXT, 9000) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
            END IF
C
C           IRET is now non-zero if the output file already exists.
C
            IF (IRET .EQ. 0) THEN
C
C              Attempt to create the output file and open it for
C              writing:
C
               CALL FXCREA (INFILE, OUTFIL, IRET)
C
               IF (IRET .EQ. 0) THEN
C
C                 The output file was created successfully.
C
C                 Transfer the contents of the input file to the output
C                 file:
C
                  CALL FXCOPY (INFILE, OUTFIL, IRET)
C
C                 Clear the destroy-on-fail status flag on the output
C                 file if no errors have been detected otherwise leave
C                 it set so that DIE will delete the file (which may be
C                 corrupt).
C
                  IF (IRET .EQ. 0) THEN
                     CALL OUCDES (OUTFIL, '    ', IRET)
                     IF (IRET .NE. 0) THEN
                        WRITE (MSGTXT, 9001) IRET
                        CALL MSGWRT (9)
                     END IF
                  END IF
               END IF
            END IF
         ELSE
            WRITE (MSGTXT, 9002) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXOUT: UNABLE TO SEARCH CATALOGUE (ERROR ', I4, ')')
 9001 FORMAT ('FXOUT: UNABLE TO CLEAR OUTPUT FILE STATUS (ERROR ', I4,
     *        ')')
 9002 FORMAT ('FXOUT: UNABLE TO CREATE UVDATA OBJECT (ERROR ', I4, ')')
      END
      SUBROUTINE FXPMAP (BNDPOL, IRET)
C-----------------------------------------------------------------------
C   Establish a mapping from input IF numbers to output polarizations
C   from BNDPOL. BNDPOL should comprise one or more repeat groups, each
C   of which is either a single letter from the set {'R', 'L', 'X', 'Y'}
C   preceded by an optional repeat count or a parenthesized sequence of
C   one more letters from this set preceded by an optional repeat count.
C
C   Input:
C     BNDPOL   C*(*)    Polarization specification string
C
C   Output:
C     IRET     I        Return status - 0 if BNDPOL is valid, non-zero
C                                       otherwise
C
C   Postconditions:
C      If IRET is zero then the mapping has been established.
C      If IRET is not zero then BNDPOL was invalid and a diagnostic
C      message has been issued.
C-----------------------------------------------------------------------
      CHARACTER BNDPOL*(*)
      INTEGER   IRET
C
C  Local variables:
C
C     START    Index of the start of the next repeat group to examine in
C              BDNPOL
C     LENGTH   Length of BNDPOL excluding trailing blanks
C     LGROUP   Number of characters in current repeat group
C
      INTEGER   START
      INTEGER   LENGTH
      INTEGER   LGROUP
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     Initialize the polarization mapping to map 0 IFs.
C
      NSTK = 0
C
      START  = 1
      LENGTH = ITRIM (BNDPOL)
      IRET   = 0
C
C     Invariant: IRET = 0 implies that BNDPOL(1:START-1) is valid
C                and that the polarization mapping corresponds to
C                BNDPOL(1:START-1)
C     Bound: LENGTH - START
C
   10    IF ((IRET .EQ. 0) .AND. (START .LE. LENGTH)
     *       .AND. (NSTK .NE. MAXIF)) THEN
C
C           Process a repeat group starting with character START in
C           BNDPOL and set IRET to zero if this group is valid or to
C           a non-zero value if it is invalid:
C
            CALL RPTGRP (BNDPOL(START:LENGTH), LGROUP, IRET)
C
C           Restore invariant:
C
            START = START + LGROUP
         GO TO 10
         END IF
C
      END
      SUBROUTINE FXBMAP (INFILE, FQTOL, IRET)
C-----------------------------------------------------------------------
C   Establish a band mapping from input IFs to output IFs and
C   polarizations and write a summary of it to the message system.
C
C   Inputs:
C     INFILE   C*(*)    Name of UVDATA object used to access input file
C     FQTOL    R        Frequency tolerance (must be positive)
C
C   Output:
C     IRET     I        Return status: zero if mapping established,
C                                      non-zero otherwise
C
C   Preconditions:
C      INFILE is the name of a UVDATA object that designates a file
C      that is opened for reading.
C      FQTOL is positive.
C      A mapping from IF numbers to output polarizations has been
C      established.
C
C   Postconditions:
C      If IRET is zero then the band mapping has been established and
C      the summary messages have been written.
C      If IRET is not zero then at least one error message has been
C      issued.
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      REAL      FQTOL
      INTEGER   IRET
C
C   Local Variables:
C
C     FQTAB    Name of TABLE object used to access FQ table (constant)
C     FQVER    Version number of FQ table (constant)
C
C     NKEYS    Number of keywords to look up in table header
C     KEYS     Keywords to look up in table header
C     KVALS    Array of keyword values
C     KLOCS    Array of indices into KVALS
C     KTYPE    Array of keyword value type codes
C
C     NUMIF    Number of IFs in FQ table (assumed to be the same as the
C              dimension of the IF axis in the main data)
C     BAND     Input band or IF number
C
C     NCOL     Number of columns to find in the FQ table
C     COLLAB   Array of column names to find in FQ table
C     COLNUM   Array of column numbers found in FQ table
C
C     IRET1    Alternate return status
C
      CHARACTER FQTAB*8
      PARAMETER (FQTAB = 'FQ table')
      INTEGER   FQVER
      PARAMETER (FQVER = 1)
C
      INTEGER   NKEYS
      PARAMETER (NKEYS = 1)
      CHARACTER KEYS(NKEYS)*8
      INTEGER   KVALS(NKEYS)
      INTEGER   KLOCS(NKEYS)
      INTEGER   KTYPE(NKEYS)
C
      INTEGER   NUMIF
      INTEGER   BAND
C
      INTEGER   NCOL
      PARAMETER (NCOL = 1)
      CHARACTER COLLAB(NCOL)*24
      INTEGER   COLNUM(NCOL)
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C
      DATA KEYS / 'NO_IF   ' /
      DATA COLLAB / 'IF FREQ                 ' /
C-----------------------------------------------------------------------
C
C     The band mapping is generated using frequency offset data from the
C     FQ table.
C
      CALL UV2TAB (INFILE, FQTAB, 'FQ', FQVER, IRET)
      IF (IRET .EQ. 0) THEN
C
C        The FQTAB object was allocated successfully:
C
         CALL TABOPN (FQTAB, 'READ', IRET)
C
         IF (IRET .EQ. 0) THEN
C
C           FQTAB is open for reading.
C
C           Attempt to read the number of IFs from the table header:
C
            CALL TABKGT (FQTAB, KEYS, NKEYS, KLOCS, KVALS, KTYPE, IRET)
            IF ((IRET .EQ. 0) .AND. (KLOCS(1) .GT. 0)) THEN
C
C              KVALS(KLOCS(1)) is the value of the NO_IF keyword.
C
               NUMIF = KVALS(KLOCS(1))
C
C              Check that the polarization map covers all of the IFs in
C              the input file:
C
               IF (NUMIF .LE. NSTK) THEN
C
C                 All IFs have defined output polarizations.
C
C                 Try to omplete the polarization mapping:
C
                  CALL SETSTK(NUMIF, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    The polarization mapping is complete.
C
C                    Look for the 'IF FREQ' column in the FQ table:
C
                     CALL TABCOL (FQTAB, NCOL, COLLAB, COLNUM, IRET)
                     IF (IRET .EQ. 0) THEN
C
C                       COLNUM(1) is the column number of the 'IF FREQ'
C                       column.
C
C                       Set up the mapping from input IFs to output IFs,
C                       if possible:
C
                        CALL FXBMA2 (FQTAB, COLNUM(1), NUMIF, FQTOL,
     *                               IRET)
                        IF (IRET .EQ. 0) THEN
C
C                          The mapping from input IFs to output IFs and
C                          polarizations is complete.
C
C                          Write out a summary of the mapping:
C
                           MSGTXT = 'Input IFs will be mapped to '
     *                              // 'new IFs and polarizations'
                           CALL MSGWRT (4)
                           MSGTXT = 'as follows.'
                           CALL MSGWRT (4)
                           DO 10 BAND = 1, NUMIF
                              WRITE (MSGTXT, 1000)
     *                           BAND, IFINDX(BAND), POLSTR(BAND:BAND)
                              CALL MSGWRT (4)
   10                      CONTINUE
C
                        END IF
                     ELSE
                           MSGTXT = 'FXBMAP: FREQUENCY TABLE IS BAD - '
     *                           // 'NO IF FREQ COLUMN'
                           CALL MSGWRT (9)
                        END IF
                     END IF
                  ELSE
                     WRITE (MSGTXT, 9010) NSTK, NUMIF
                     CALL MSGWRT (9)
                     MSGTXT = 'FXBMAP: CHECK THE VALUE OF THE BANDPOL '
     *                        // 'ADVERB'
                     CALL MSGWRT (9)
                     IRET = 1
                  END IF
C
            ELSE
               MSGTXT = 'FXBMAP: FREQUENCY TABLE IS BAD - NO NO_IF '
     *                  // 'KEYWORD'
               CALL MSGWRT (9)
            END IF
C
C           If it is possible to close the FQ table then close it
C           otherwise set IRET to a non-zero value and issue an error
C           message:
C
C           IRET1 is used to avoid clearing any existing error
C           indication in IRET if the table is closed successfully.
C
            CALL TABCLO (FQTAB, IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9011) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           FQTAB could not be opened.
C
            WRITE (MSGTXT, 9012) IRET
            CALL MSGWRT (9)
         END IF
C
C        If it is possible to delete the FQTAB object then destroy it
C        otherwise set IRET to a non-zero value and issue an error
C        message:
C
         CALL TABDES (FQTAB, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9013) IRET1
            CALL MSGWRT (9)
            IRET = IRET1
         END IF
C
      ELSE
C
C        Could not create the FQTAB object:
C
         WRITE (MSGTXT, 9014) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 1000 FORMAT ('    ', I4, ' -> ', I4, A1)
 9010 FORMAT ('FXBMAP: ONLY HAVE POLARIZATIONS FOR ', I4,
     *        ' IFS OUT OF ', I4)
 9011 FORMAT ('FXBMAP FAILED TO CLOSE FQ TABLE (ERROR ', I4, ')')
 9012 FORMAT ('FXBMAP: FAILED TO OPEN FQ TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXBMAP: FAILED TO DESTROY FQ TABLE OBJECT (ERROR ', I4,
     *        ')')
 9014 FORMAT ('FXBMAP: FAILED TO CREATE FQ TABLE OBJECT (ERROR ', I4,
     *        ')')
      END
      SUBROUTINE FXCREA (INFILE, OUTFIL, IRET)
C-----------------------------------------------------------------------
C   Create and open the output file using INFILE as a template.
C
C   Intended Function:
C
C   Inputs:
C     INFILE   C*(*)    Name of UVDATA object used to access input file
C     OUTFIL   C*(*)    Name of UVDATA object used to access output file
C
C   Output:
C     IRET     I        Return status: 0 if file created and opened,
C                                      non-zero otherwise
C
C   Preconditions:
C      INFILE is open for reading.
C      The FILE_NAME attributes of OUTFIL have been set.
C      A mapping has been established from input IFs to output IFs and
C      polarizations.
C
C   Postconditions:
C      If IRET is zero then the output file has been created, is open
C      for writing, and has destroy-on-fail status.
C      If IRET is not zero then at least one error message has been
C      issued.
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   IRET
C
C  Local variables:
C
C     NAXIS    Array of axis dimensions
C     CRVAL    Array of axis reference values
C     CDELT    Array of axis coordinate increments
C     STKAXI   Index of STOKES axis in NAXIS, CRVAL, and CDELT
C     IFAXIS   Index of IF axis in NAXIS, CRVAL, and CDELT
C
C     TYPE     Attribute type code
C     DIM      Attribute array dimensions
C     CDUMMY   Dummy character argument
C
      INTEGER   NAXIS(7)
      DOUBLE PRECISION CRVAL(7)
      REAL      CDELT(7)
      INTEGER   STKAXI
      INTEGER   IFAXIS
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     The UV_DESC attributes of OUTFIL must be filled in before the
C     corresponding file is created. The strategy used here is to copy
C     the UV_DESC attributes from INFILE and then to correct the STOKES
C     and IF axis definitions for the output file.
C
      CALL UVDCOP (INFILE, OUTFIL, IRET)
      IF (IRET .EQ. 0) THEN
C
C        The descriptor was copied successfully.
C
         CALL UVDGET (OUTFIL, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IRET)
         CALL UVDGET (OUTFIL, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IRET)
         CALL UVDGET (OUTFIL, 'CDELT', TYPE, DIM, CDELT, CDUMMY, IRET)
C
C        Look for the STOKES axis:
C
         CALL UVDFND (OUTFIL, 2, 'STOKES', STKAXI, IRET)
C
         IF (IRET .EQ. 0) THEN
C
C           STKAXI is the index of the STOKES axis.
C
            IF (NAXIS(STKAXI) .EQ. 1) THEN
               NAXIS(STKAXI) = STKDIM
               CRVAL(STKAXI) = DBLE (STK1)
               CDELT(STKAXI) = -1.0
C
C              Look for the IF axis:
C
               CALL UVDFND (OUTFIL, 2, 'IF', IFAXIS, IRET)
               IF (IRET .EQ. 0) THEN
C
C                 IFAXIS is the index of the IF axis:
C
                  NAXIS(IFAXIS) = IFDIM
C
C                 Write the updated axis definitions back to OUTFIL
C                 and set IRET to zero:
C
                  TYPE = OOAINT
                  CALL UVDPUT (OUTFIL, 'NAXIS', TYPE, DIM, NAXIS,
     *                         CDUMMY, IRET)
                  TYPE = OOADP
                  CALL UVDPUT (OUTFIL, 'CRVAL', TYPE, DIM, CRVAL,
     *                         CDUMMY, IRET)
                  TYPE = OOARE
                  CALL UVDPUT (OUTFIL, 'CDELT', TYPE, DIM, CDELT,
     *                         CDUMMY, IRET)
C
C                 Open the output file, if possible:
C
                  CALL OUVOPN (OUTFIL, 'DEST', IRET)
                  IF (IRET .NE. 0) THEN
C
C                    Could not open the file.
C
                     WRITE (MSGTXT, 9000) IRET
                     CALL MSGWRT (9)
                  END IF
               ELSE
C
C                 Could not find an IF axis.
C
                  MSGTXT = 'FXCREA: INVALID INPUT FILE - NO IF AXIS '
     *                     // 'PRESENT'
                  CALL MSGWRT (9)
               END IF
            ELSE
C
C              File already has more than 1 polarization.
C
               MSGTXT = 'FXCREA: INVALID INPUT FILE - ALREADY MORE '
     *                  // 'THAN 1 POLARIZATION'
               CALL MSGWRT (9)
               IRET = 1
            END IF
         ELSE
C
C           Could not find STOKES axis.
C
            MSGTXT = 'FXCREA: INVALID INPUT FILE - NO STOKES AXIS '
     *               // 'PRESENT'
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Failed to copy data descriptors:
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXCREA: FAILED TO OPEN OUTPUT FILE (ERROR ', I4, ')')
 9001 FORMAT ('FXCREA: FAILED TO COPY DATA DESCRIPTION (ERROR ', I4,
     *        ')')
      END
      SUBROUTINE FXCOPY (INFILE, OUTFIL, IRET)
C-----------------------------------------------------------------------
C   Copy contents of INFILE to OUTFIL.
C
C   Inputs:
C     INFILE   C*(*)       Name of UVDATA object used to access input
C                          file
C     OUTFIL   C*(*)       Name of UVDATA object used to access output
C                          file
C
C   Output:
C     IRET     I           Return status: 0 if all contents copied,
C                                         non-zero otherwise
C
C   Preconditions:
C      INFILE is open for "raw" reading.
C      OUTFIL is open for writing.
C      A mapping has been established between input IFs and output IFs
C      and polarizations.
C
C   Postconditions:
C      If IRET is zero then OUTFIL contains a transformed copy of the
C      data from INFILE, is closed, and has destroy-on-fail status.
C      If IRET is non-zero then one or more error messages have been
C      issued.
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   IRET
C
C     Local variables:
C
C     SORT     data sort order
C
C     TYPE     attribute type
C     DIM      attribute dimensions
C     NDUMMY   dummy numerical argument
C
      CHARACTER SORT*2
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      REAL      NDUMMY
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Copy keywords from INFILE to OUTFIL, if possible:
C
      CALL FXKEYW (INFILE, OUTFIL, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Keywords were copied successfully.
C
C        Copy and transform data, if possible
C
         CALL FXDATA (INFILE, OUTFIL, IRET)
C
         IF (IRET .EQ. 0) THEN
C
C           The data were copied successfully.
C
C           Copy the sort order from INFILE to OUTFIL:
C
            CALL UVDGET (INFILE, 'SORTORD', TYPE, DIM, NDUMMY, SORT,
     *                   IRET)
            CALL UVDPUT (OUTFIL, 'SORTORD', TYPE, DIM, NDUMMY, SORT,
     *                   IRET)
C
C           The output file must be closed before the tables are
C           copied otherwise the APLOOP routines will corrupt the
C           catalogue header.
C
            CALL OUVCLO (OUTFIL, IRET)
            IF (IRET .EQ. 0) THEN
C
C              The file was closed successfully.
C
C              Set the destroy-on-fail status flag which was cleared
C              when the file was closed, if possible:
C
               CALL OUCSET (OUTFIL, 'DEST', IRET)
               IF (IRET .EQ. 0) THEN
C
C                 The status flag was set successfully.
C
C                 Attempt to copy and reformat the tables:
C
                  CALL FXTABS (INFILE, OUTFIL, IRET)
C
               ELSE
C
C                 Failed to set status flag.
C
                  WRITE (MSGTXT, 9000) IRET
                  CALL MSGWRT (9)
               END IF
            ELSE
C
C              Failed to close output file.
C
               WRITE (MSGTXT, 9001) IRET
               CALL MSGWRT (9)
            END IF
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXCOPY: FAILED TO SET STATUS ON OUTPUT FILE (ERROR ', I4,
     *        ')')
 9001 FORMAT ('FXCOPY: FAILED TO CLOSE OUTPUT FILE (ERROR ', I4, ')')
      END
      SUBROUTINE RPTGRP (GROUPS, LGROUP, IRET)
C-----------------------------------------------------------------------
C   Process one repeat group from the polarization specification.
C
C   A valid repeat group consists of a polarization specification
C   preceded by an optional, positive, integral repeat count. The
C   polarization specification is either a single letter R, L, X, or Y
C   or a sequence of letters R, L, X, and Y enclosed in parentheses.
C
C   Inputs:
C     GROUPS   C*(*)    String to be processed
C
C   Outputs:
C     LGROUP   I        Length of repeat group at the begining of
C                       GROUPS
C     IRET     I        Return code: 0 if repeat group was valid
C                                    non-zero otherwise
C
C   Postconditions:
C      If IRET is zero then the first repeat group in GROUPS has been
C      processed and LGROUP is the number of characters in that repeat
C      group.
C      If IRET is not zero then an error message has been issued.
C-----------------------------------------------------------------------
      CHARACTER GROUPS*(*)
      INTEGER   LGROUP
      INTEGER   IRET
C
C   Local Variables:
C
C     LENGTH   Number of characters in GROUPS excluding trailing blanks
C     REPEAT   Number of times to repeat polarization specification
C     LPAR     Offset of closing parenthesis from first character inside
C              parentheses
C     TEST     Character to be tested
C
      INTEGER   LENGTH
      INTEGER   REPEAT
      INTEGER   LPAR
      CHARACTER TEST
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
      LENGTH = LEN (GROUPS)
      IRET   = 0
      LGROUP = 0
C
C     Set LGROUP to the number of digits in the repeat count.
C     Invariant: GROUPS(1:LGROUP) comprises only decimal digits.
C     Bound: LENGTH - LGROUP
C
   10 IF (LGROUP .LT. LENGTH) THEN
         TEST = GROUPS(LGROUP+1:LGROUP+1)
         IF ((TEST .EQ. '0') .OR. (TEST .EQ. '1')
     *       .OR. (TEST .EQ. '2') .OR. (TEST .EQ. '3')
     *       .OR. (TEST .EQ. '4') .OR. (TEST .EQ. '5')
     *       .OR. (TEST .EQ. '6') .OR. (TEST .EQ. '7')
     *       .OR. (TEST .EQ. '8') .OR. (TEST .EQ. '9')) THEN
            LGROUP = LGROUP + 1
         GO TO 10
         END IF
      END IF
C
C     Set REPEAT to the number of times to repeat the group:
C
      IF (LGROUP .GT. 0) THEN
         READ (GROUPS(1:LGROUP), 1010) REPEAT
      ELSE IF (GROUPS(LGROUP+1:LGROUP+1) .EQ. '*') THEN
         REPEAT = MAXIF
         LGROUP = LGROUP + 1
      ELSE
         REPEAT = 1
      END IF
C
      IF (REPEAT .GT. 0) THEN
         IF (LGROUP .LT. LENGTH) THEN
            TEST = GROUPS(LGROUP+1:LGROUP+1)
            IF (TEST .EQ. '(') THEN
C
C              The body of the group is a parenthesized list.
C
C              Process the parenthesized group:
C
               CALL PARENS (REPEAT, GROUPS(LGROUP+2:LENGTH), LPAR, IRET)
C
C              Set LGROUP to the index of the last character in the
C              repeat group:
C
               LGROUP = LGROUP + LPAR + 1
C
            ELSE IF ((TEST .EQ. 'R') .OR. (TEST .EQ. 'L')
     *               .OR. (TEST .EQ. 'X') .OR. (TEST .EQ. 'Y')) THEN
C
C              The body of the group is a single character.
C
C              Append the requested polarization to the polarization
C              map:
C
               CALL APDPOL (REPEAT, GROUPS(LGROUP+1:LGROUP+1))
C
C              Set LGROUP to the index of the last character in the
C              repeat group:
C
               LGROUP = LGROUP + 1
C
            ELSE
C
C              The repeat count is followed by an invalid character.
C
               WRITE (MSGTXT, 9010)
               CALL MSGWRT (9)
               WRITE (MSGTXT, 9011) TEST
               CALL MSGWRT (9)
               IRET = 1
            END IF
         ELSE
C
C           There are no characters following the repeat count.
C
            WRITE (MSGTXT, 9010)
            CALL MSGWRT (9)
            MSGTXT = 'RPTGRP: STRING CAN NOT END WITH A NUMBER'
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE
C
C        The repeat count is zero.
C
         WRITE (MSGTXT, 9010)
         CALL MSGWRT (9)
         MSGTXT = 'RPTGRP: REPEAT COUNTS MUST BE GREATER THAN ZERO'
         CALL MSGWRT (9)
         IRET = 1
      END IF
C-----------------------------------------------------------------------
 1010 FORMAT (I10)
 9010 FORMAT ('RPTGRP: POLARIZATION STRING ERROR')
 9011 FORMAT ('RPTGRP: CHARACTER ', A1, ' NOT ALLOWED')
      END
      SUBROUTINE SETSTK (NUMIF, IRET)
C-----------------------------------------------------------------------
C   Set the polarization mapping function from POLSTR (in common) and
C   check for mixed linear/circular polarizations.
C
C   Input:
C     NUMIF    I     Number of IFs to be mapped.
C
C   Output:
C     IRET     I     Return status: 0 if polarization mapping was set
C                                   non-zero otherwise
C
C   Preconditions:
C      NUMIF <= NSTK (in common)
C
C   Postconditions:
C      If IRET is zero then
C      - STKDIM is the dimension of the output STOKES axis
C      - STK1 is the reference vlaue for the STOKES axis
C      - STKIDX(1:NUMSTK) is the output STOKES value for each input IF
C      If IRET is not zero then an error message has been issued.
C-----------------------------------------------------------------------
      INTEGER   NUMIF
      INTEGER   IRET
C
C     Local Variables:
C
C     CHAR  character index
C
      INTEGER   CHAR
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (INDEX (POLSTR(1:NUMIF), 'R') .GT. 0) THEN
         STK1 = -1
         IF ((INDEX (POLSTR(1:NUMIF), 'X') .GT. 0)
     *       .OR. (INDEX (POLSTR(1:NUMIF), 'Y') .GT. 0)) THEN
C
C           POLSTR specifies both linear and circular polarization.
C
            WRITE (MSGTXT, 9000)
            CALL MSGWRT (9)
            WRITE (MSGTXT, 9001)
            CALL MSGWRT (9)
            IRET = 1
         ELSE IF (INDEX (POLSTR(1:NUMIF), 'L') .GT. 0) THEN
C
C           POLSTR specifies both L and R polarizations.
C
            STKDIM = 2
            DO 10 CHAR = 1, NUMIF
               IF (POLSTR(CHAR:CHAR) .EQ. 'R') THEN
                  STKIDX(CHAR) = 1
               ELSE
                  STKIDX(CHAR) = 2
               END IF
   10       CONTINUE
         ELSE
C
C           POLSTR specifies RCP only.
C
            STKDIM = 1
            CALL FILL (NUMIF, 1, STKIDX)
         END IF
      ELSE IF (INDEX (POLSTR(1:NUMIF), 'L') .GT. 0) THEN
C
C        Mixed RCP and LCP was covered in the previous branch.
C
         STK1   = -2
         STKDIM = 1
         CALL FILL (NUMIF, 1, STKIDX)
         IF ((INDEX (POLSTR(1:NUMIF), 'X') .GT. 0)
     *       .OR. (INDEX (POLSTR(1:NUMIF), 'Y') .GT. 0)) THEN
C
C           POLSTR mixes linear and circular polarizations.
C
            WRITE (MSGTXT, 9000)
            CALL MSGWRT (9)
            WRITE (MSGTXT, 9001)
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE IF (INDEX (POLSTR(1:NUMIF), 'X') .GT. 0) THEN
C
C        Previous branches have dealt with circular polarizations so it
C        is no longer necessary to check for mixed polarizations.
C
         STK1 = -5
         IF (INDEX (POLSTR(1:NUMIF), 'Y') .GT. 0) THEN
            STKDIM = 2
            DO 20 CHAR = 1, NUMIF
               IF (POLSTR(CHAR:CHAR) .EQ. 'X') THEN
                  STKIDX(CHAR) = 1
               ELSE
                  STKIDX(CHAR) = 2
               END IF
   20       CONTINUE
         ELSE
            STKDIM = 1
            CALL FILL (NUMIF, 1, STKIDX)
         END IF
      ELSE
C
C        All possibilities except all Y have been eliminated.
C
         STK1   = -6
         STKDIM = 1
         CALL FILL (NUMIF, 1, STKIDX)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('SETSTK: POLARIZATION STRING ERROR')
 9001 FORMAT ('SETSTK: YOU MAY NOT MIX LINEAR AND CIRCULAR ',
     *        'POLARIZATIONS')
      END
      SUBROUTINE FXBMA2 (FQTAB, COLNUM, NUMIF, FQTOL, IRET)
C-----------------------------------------------------------------------
C   Complete frequency band mapping and check for consecutive IFs with
C   the same frequency and polarization and for different frequency
C   structures in different FQ table entries.
C
C   Inputs:
C     FQTAB    C*(*)    Name of TABLE object used to access FQ table
C     COLNUM   I        Number of 'IF FREQ' column
C     NUMIF    I        Number of IFs
C     FQTOL    R        Frequency tolerance for frequency comparisons
C                       in Hz
C
C   Output:
C     IRET     I        Return status: 0 if mapping completed
C                                      non-zero otherwise
C
C   Preconditions:
C      FQTAB is open for reading.
C      The polarization mapping has been established.
C      COLNUM is the number of the 'IF FREQ' column in FQTAB.
C      NUMIF is the number of IFs in FQTAB.
C      FQTOL > 0.0
C
C   Postconditions:
C      If IRET is zero then
C      - IFDIM is the number of output IFs
C      - IFINDX(1:NUMIF) is the output IF number for each input IF.
C      If IRET is not zero then an error message has been issued.
C-----------------------------------------------------------------------
      CHARACTER FQTAB*(*)
      INTEGER   COLNUM
      INTEGER   NUMIF
      REAL      FQTOL
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     OFFSET   Frequency offset array
C
C     IN       Input IF number
C     OUT      Output IF number
C
C     ROW      Row number
C     NUMROW   Number of rows in table
C
C     TYPE     Attribute type code
C     DIM      Attribute dimensions
C     CDUMMY   Dummy character argument
C
      DOUBLE PRECISION OFFSET(MAXIF)
C
      INTEGER   IN
      INTEGER   OUT
C
      INTEGER   ROW
      INTEGER   NUMROW
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     Read the frequency offsets from the first row of the table into
C     OFFSET, if possible:
C
      CALL TABDGT (FQTAB, 1, COLNUM, TYPE, DIM, OFFSET, CDUMMY, IRET)
      IF (IRET .EQ. 0) THEN
C
C     Map input IFs to output IFs and check for duplicated polarizations
C     and offsets:
C
         IN         = 1
         IFINDX(IN) = 1
         IFDIM      = 1
C
C     Invariant: IRET = 0 implies that IFINDX(1:IN) contains output IFs
C     for input IFs 1 to IN and IFDIM = IFINDX(IN)
C     Bound: NUMIF - IN
C
   10    IF ((IRET .EQ. 0) .AND. (IN .LT. NUMIF)) THEN
            IN = IN + 1
            IF (ABS (OFFSET(IN) - OFFSET(IN - 1)) .LE. FQTOL) THEN
               IF (STKIDX(IN) .EQ. STKIDX(IN - 1)) THEN
                  WRITE (MSGTXT, 9010)
                  CALL MSGWRT (9)
                  WRITE (MSGTXT, 9011) IN - 1, IN
                  CALL MSGWRT (9)
                  IRET = 1
               END IF
            ELSE
C
C           A new output IF is needed.
C
               IFDIM = IFDIM + 1
            END IF
            IFINDX(IN) = IFDIM
         GO TO 10
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Check that the remaining rows have the same frequency
C           structure:
C
            ROW = 1
            CALL TABGET (FQTAB, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
C           Invariant: IRET = 0 implies that rows 2 through ROW have the
C                      same frequency structure as row 1
C           Bound: NUMROW - ROW
C
   20       IF ((IRET .EQ. 0) .AND. (ROW .LT. NUMROW)) THEN
               ROW = ROW + 1
               CALL TABDGT (FQTAB, ROW, COLNUM, TYPE, DIM, OFFSET,
     *                      CDUMMY, IRET)
               IF (IRET .EQ. 0) THEN
C
C                 OFFSET contain frequency offsets from row ROW.
C
                  IN  = 1
                  OUT = 1
C
C                 Invariant: IFs 1 through IN have the same relationship
C                            in row ROW as in row 1
C                 Bound: NUMIF - IN
C
   30             IF ((IRET .EQ. 0) .AND. (IN .LT. NUMIF)) THEN
                     IN = IN + 1
                     IF (ABS (OFFSET(IN) - OFFSET(IN - 1))
     *                   .GT. FQTOL) THEN
                        OUT = OUT + 1
                     END IF
                     IF (OUT .NE. IFINDX(IN)) THEN
                        MSGTXT = 'FXBMA2: CAN NOT APPLY THE SAME '
     *                           // 'MAPPING TO ALL FQIDS'
                        CALL MSGWRT (9)
                        MSGTXT = 'FXBMA2: CAN NOT CONTINUE'
                        CALL MSGWRT (9)
                        IRET = 1
                     END IF
                  GO TO 30
                  END IF
               ELSE
C
C                 Failed to read from the table.
C
                  WRITE (MSGTXT, 9030) IRET
                  CALL MSGWRT (9)
               END IF
            GO TO 20
            END IF
         END IF
C
      ELSE
C
C        Failed to read from the table.
C
         WRITE (MSGTXT, 9030) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9010 FORMAT ('FXBMA2: YOU CAN NOT ASSIGN THE SAME POLARIZATION TO ',
     *        'IFS WITH')
 9011 FORMAT ('FXBMA2: SAME FREQUENCY (IF NUMBERS ', I4, ' AND ', I4,
     *        ')')
 9030 FORMAT ('FXBMA2: FAILED TO READ FQ TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FXKEYW (INFILE, OUTFIL, IRET)
C-----------------------------------------------------------------------
C   Copy keywords from INFILE to OUTFIL.
C
C   Intended Function:
C
C     [ INFILE names a UVDATA object that is open for reading
C       and OUTFIL names a UVDATA object that is open for writing
C           [ all header keywords can be copied from INFILE to OUTFIL ->
C                 OUTFIL, IRET :=
C                    OUTFIL + copies of keyword/value pairs from INFILE,
C                    0
C           | else ->
C                 OUTFIL, messages, IRET :=
C                    any, messages + fatal error message, non-zero ]
C     ]
C
C   Inputs:
C     INFILE   C*(*)    Name of UVDATA object used to access input file
C     OUTFIL   C*(*)    Name of UVDATA object used to access output
C                       file
C
C   Output:
C     IRET     I        Return status: 0 if keywords copied,
C                                      non-zero otherwise
C
C   Preconditions:
C      INFILE is the name of a UVDATA object that is open for reading
C      OUTFIL is the name of a UVDATA object that is open for writing
C
C   Postconditions:
C      If IRET is zero then all of the keywords (up to a maximum of 50)
C      have been copied from INFILE to OUTFIL.
C      If IRET is not zero then at least one error message has been
C      issued.
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   IRET
C
C     Local Variables:
C
C     INDISK   disk number for input file
C     INCNO    catalogue number for input file
C     OUTDSK   disk number for output file
C     OUTCNO   catalogue number for output file
C
C     NUMKEY   number of keywords to copy
C     MAXKEY   maximum number of keywords expected
C     KEYWRD   list of keywords
C     LOCS     keyword pointer array
C     VALUES   keyword values array
C     KEYTYP   keyword type code array
C     BUFFER   I/O buffer
C
C     TYPE     Attribute type code
C     DIM      Attribute array dimensions
C     CDUMMY   Dummy character argument
C
      INTEGER   INDISK
      INTEGER   INCNO
      INTEGER   OUTDSK
      INTEGER   OUTCNO
C
      INTEGER   NUMKEY
      INTEGER   MAXKEY
      PARAMETER (MAXKEY = 50)
      CHARACTER KEYWRD(MAXKEY)*8
      INTEGER   LOCS(MAXKEY)
      DOUBLE PRECISION VALUES(MAXKEY)
      INTEGER   KEYTYP(MAXKEY)
      INTEGER   BUFFER(256)
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NUMKEY = MAXKEY
C
C     Get disk and catalogue numbers and set IRET to zero
C
      CALL FNAGET (INFILE, 'DISK', TYPE, DIM, INDISK, CDUMMY, IRET)
      CALL FNAGET (INFILE, 'CNO', TYPE, DIM, INCNO, CDUMMY, IRET)
      CALL FNAGET (OUTFIL, 'DISK', TYPE, DIM, OUTDSK, CDUMMY, IRET)
      CALL FNAGET (OUTFIL, 'CNO', TYPE, DIM, OUTCNO, CDUMMY, IRET)
C
C     Read the keywords from the input file, if possible:
C
      CALL CATKEY ('ALL ', INDISK, INCNO, KEYWRD, NUMKEY, LOCS, VALUES,
     *             KEYTYP, BUFFER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Keywords were read successfully.
C
         IF (NUMKEY .GT. 0) THEN
C
C           Write the keywords to the output file, if possible:
C
            CALL CATKEY ('WRIT', OUTDSK, OUTCNO, KEYWRD, NUMKEY, LOCS,
     *                   VALUES, KEYTYP, BUFFER, IRET)
C
            IF (IRET .NE. 0) THEN
C
C              Failed to write keywords:
C
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
      ELSE
C
C        Failed to read keywords:
C
         WRITE (MSGTXT, 9000) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXKEYW: FAILED TO COPY HEADER KEYWORDS (ERROR ', I4, ')')
      END
      SUBROUTINE FXDATA (INFILE, OUTFIL, IRET)
C-----------------------------------------------------------------------
C   Copy data from INFILE to OUTFIL, reformatting on the fly.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output file
C
C   Output:
C     IRET     I        return status: 0 if data transferred
C                                      non-zero otherwise
C
C   Preconditions:
C      INFILE is open for reading
C      OUTFIL is open for writing and is empty
C      A mapping from input IFs to output IFs and polarizations has
C      been established.
C
C   Postconditions:
C      If IRET is zero then OUTFIL contains a transformed copy of the
C      data in INFILE.
C      If IRET is not zero the at least one error message has been
C      issued.
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   IRET
C
C     Local Variables:
C
C     NDIM     number of data array axes
C     INAXIS   axis dimensions for input data array
C     OUTAXI   axis dimensions for output data array
C     NREC     number of records to transfer
C     INLEN    size of inpur data array
C     OUTLEN   size of output data array
C
C     INDATA   base of allocated input data array
C     INDOFF   offset of allocated input data array
C     LOOKUP   base of allocated output index lookup table
C     LKOFF    offset of allocated index lookup table
C     OUTDAT   base of allocated output data array
C     OUTOFF   offset of allocated output data array
C
C     AXIS     axis number
C
C     STKIND   index number of STOKES axis
C     IFIND    index number of IF axis
C
C     TYPE     attribute type code
C     DIM      attribute array dimensions
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C     IRET2    alternate return status
C     IRET3    alternate return status
C
      INTEGER   NDIM
      INTEGER   INAXIS(7)
      INTEGER   OUTAXI(7)
      INTEGER   NREC
      INTEGER   INLEN
      INTEGER   OUTLEN
C
      REAL      INDATA(2)
      INTEGER   LOOKUP(2)
      REAL      OUTDAT(2)
      LONGINT   INDOFF, LKOFF, OUTOFF
C
      INTEGER   AXIS, NWORDS
C
      INTEGER   STKIND
      INTEGER   IFIND
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
      INTEGER   IRET2
      INTEGER   IRET3
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     The bulk of the work is done in FXDAT2. FXDATA allocates workspace
C     for FXDAT2 and sets up the mapping from input data to output data.
C
      CALL UVDGET (INFILE, 'NDIM', TYPE, DIM, NDIM, CDUMMY, IRET)
      CALL UVDGET (INFILE, 'NAXIS', TYPE, DIM, INAXIS, CDUMMY, IRET)
      CALL UVDGET (OUTFIL, 'NAXIS', TYPE, DIM, OUTAXI, CDUMMY, IRET)
      CALL UVDGET (OUTFIL, 'GCOUNT', TYPE, DIM, NREC, CDUMMY, IRET)
C
C     INAXIS(1) and OUTAXI(1) will each be 1 if the data is
C     compressed but the data arrays will need to hold uncompressed
C     data.
C
      INAXIS(1) = 3
      OUTAXI(1) = 3
C
C     Set INLEN to the product of INAXIS(1:NDIM) and OUTLEN to the
C     product of OUTAXI(1:NDIM):
C
      INLEN  = 1
      OUTLEN = 1
C
C     Invariant: INLEN is the product of INAXIS(1:AXIS-1) and OUTLEN
C                is the product of OUTAXI(1:AXIS-1)
C
      DO 10 AXIS = 1, NDIM
         INLEN  = INLEN * INAXIS(AXIS)
         OUTLEN = OUTLEN * OUTAXI(AXIS)
   10 CONTINUE
C
C     Allocate work space, if possible
C
      NWORDS = (INLEN - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'FXDATA', NWORDS, INDATA, INDOFF, IRET1)
      CALL ZMEMRY ('GET ', 'FXDATA', NWORDS, LOOKUP, LKOFF, IRET2)
      NWORDS = (OUTLEN - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'FXDATA', NWORDS, OUTDAT, OUTOFF, IRET3)
C
      IF ((IRET1 .EQ. 0) .AND. (IRET2 .EQ. 0) .AND. (IRET3 .EQ. 0)) THEN
C
C        Work space was allocated successfully.
C
         CALL UVDFND (INFILE, 2, 'STOKES', STKIND, IRET)
         IF (IRET .EQ. 0) THEN
C
C           STKIND is the index of the STOKES axis.
C
            CALL UVDFND (INFILE, 2, 'IF', IFIND, IRET)
            IF (IRET .EQ. 0) THEN
C
C              IFIND is the index of the IF axis.
C
C              Set LOOKUP(LKOFF+1:LKOFF+INLEN) to the output data
C              indices corresponding to input data indices 1 to INLEN:
C
               CALL FXLKUP (NDIM, STKIND, IFIND, INAXIS, OUTAXI, INLEN,
     *                      LOOKUP(LKOFF + 1))
C
C              Copy the data, if possible:
C
               CALL FXDAT2 (NREC, INLEN, LOOKUP(LKOFF + 1), INFILE,
     *                      INDATA(INDOFF + 1), OUTLEN, OUTFIL,
     *                      OUTDAT(OUTOFF + 1), IRET)
C
            ELSE
C
C              Could not find an IF axis.
C
               MSGTXT = 'FXDATA: DATA MUST HAVE AN IF AXIS'
               CALL MSGWRT (9)
            END IF
         ELSE
C
C           Could not find a STOKES axis.
C
            MSGTXT = 'FXDATA: DATA MUST HAVE A STOKES AXIS'
            CALL MSGWRT (9)
         END IF
C
C        Release allocated memory, if possible, otherwise issue an error
C        message and set IRET to a non-zero value.
C
         CALL ZMEMRY ('FRAL', 'FXDATA', INLEN, INDATA, INDOFF, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9010) IRET1
            CALL MSGWRT (9)
            IRET = IRET1
         END IF
C
      ELSE
C
C        Failed to allocate one or more memory segments.
C
         MSGTXT = 'FXDATA: FAILED TO ALLOCATE MEMORY FOR I/O BUFFERS'
         CALL MSGWRT (9)
         IRET = 1
      END IF
C-----------------------------------------------------------------------
 9010 FORMAT ('FXDATA: FAILED TO FREE MEMORY (ERROR ', I4, ')')
      END
      SUBROUTINE FXTABS (INFILE, OUTFIL, IRET)
C-----------------------------------------------------------------------
C   Read tables from INFILE, transform them, and write them to OUTFIL.
C
C   If INFILE is a UVDATA object, OUTFIL is a UVDATA object, a
C   polarization and IF mapping has been established to transform INFILE
C   to OUTFIL, and INFILE and OUTFIL do not refer to the same data file
C   then, if all of the tables belonging to INFILE can be transformed
C   and written to OUTFIL, attach transformed copies of the tables
C   belonging to INFILE to OUTFIL with the same version numbers and set
C   IRET to zero or, if all of  tables can not be transformed and copied
C   to OUTFIL, transform any of the tables attached to INFILE and write
C   them to OUTFIL, issue one or more error messages, and set IRET to a
C   non-zero value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output file
C
C   Output:
C     IRET     I        return status: 0 if all tables transferred
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   IRET
C
C   Local Variables:
C
C     CATBLK   catalogue header block from input file
C     TABTYP   current table type
C     TAB      current table type slot number
C     TABVER   current table version
C
      INTEGER   CATBLK(256)
      CHARACTER TABTYP*2
      INTEGER   TAB
      INTEGER   TABVER
C
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     The strategy is to loop through all of the extension types listed
C     in the catalogue block. Each recognized table is passed to a
C     specialized routine that can reformat that particular table.
C     Unrecognized table types are discarded. Additional table types
C     can be added to the block-IF statement below as needed.
C
C     Retrieve the catalogue block:
C
      CALL OUVCGT (INFILE, CATBLK, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Examine each extension slot in turn until all slots have been
C        examined or until an error is detected:
C
         TAB = 0
   10    IF ((IRET .EQ. 0) .AND. (TAB .NE. KIEXTN)) THEN
C
C           Set TABTYP to the table type code for extension slot
C           TAB + 1, set TABVER to the highest table version number for
C           that table type and increment TAB by 1:
C
            CALL H2CHR (2, 1, CATBLK(KHEXT + TAB), TABTYP)
            TABVER = CATBLK(KIVER + TAB)
            TAB = TAB + 1
C
            IF (TABTYP .EQ. 'AN') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
   20          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has an antenna table with version
C                 number TABVER than attach a reformatted copy of the
C                 table to OUTFIL as CL table TABVER and set IRET to 0
C                 or issue one or more error messages and set IRET to a
C                 non-zero value if this is not possible.
C                 If the input file does not have an antenna table with
C                 version number TABVER then set IRET to 0.
C
                  CALL FIXPAN (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 20
               END IF
            ELSE IF (TABTYP .EQ. 'CL') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
   30          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a calibration table with
C                 version number TABVER than attach a reformatted copy
C                 of the table to OUTFIL as OB table TABVER and set IRET
C                 to 0 or issue one or more error messages and set IRET
C                 to a non-zero value if this is not possible.
C                 If the input file does not have a calibration table
C                 with version number TABVER then set IRET to 0.
C
                  CALL FIXPCL (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 30
               END IF
            ELSE IF (TABTYP .EQ. 'CQ') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
   40          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a CQ table with version number
C                 TABVER than attach a reformatted copy of the table to
C                 OUTFIL as CQ table TABVER and set IRET to 0 or issue
C                 one or more error messages and set IRET to a non-zero
C                 value if this is not possible.
C                 If the input file does not have a CQ table with
C                 version number TABVER then set IRET to 0.
C
                  CALL FIXPCQ (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 40
               END IF
            ELSE IF (TABTYP .EQ. 'CT') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
   50          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a CT table with version number
C                 TABVER than attach a reformatted copy of the table to
C                 OUTFIL as CT table TABVER and set IRET to 0 or issue
C                 one or more error messages and set IRET to a non-zero
C                 value if this is not possible.
C                 If the input file does not have a CT table with
C                 version number TABVER then set IRET to 0.
C
                  CALL FIXPCT (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 50
               END IF
            ELSE IF (TABTYP .EQ. 'FG') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
   60          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a flag table with version number
C                 TABVER than attach a reformatted copy of the table to
C                 OUTFIL as FG table TABVER and set IRET to 0 or issue
C                 one or more error messages and set IRET to a non-zero
C                 value if this is not possible.
C                 If the input file does not have a flag table with
C                 version number TABVER then set IRET to 0.
C
                  CALL FIXPFG (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 60
               END IF
            ELSE IF (TABTYP .EQ. 'FQ') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
   70          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a frequency table with version
C                 number TABVER than attach a reformatted copy of the
C                 table to OUTFIL as FQ table TABVER and set IRET to 0
C                 or issue one or more error messages and set IRET to a
C                 non-zero value if this is not possible.
C                 If the input file does not have a frequency table with
C                 version number TABVER then set IRET to 0.
C
                  CALL FIXPFQ (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 70
               END IF
            ELSE IF (TABTYP .EQ. 'GC') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
   80          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a gain curve table with version
C                 number TABVER than attach a reformatted copy of the
C                 table to OUTFIL as GC table TABVER and set IRET to 0
C                 or issue one or more error messages and set IRET to a
C                 non-zero value if this is not possible.
C                 If the input file does not have a gain curve table
C                 with version number TABVER then set IRET to 0.
C
                  CALL FIXPGC (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 80
               END IF
            ELSE IF (TABTYP .EQ. 'IM') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
   90          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has an interferometer model table
C                 with version number TABVER than attach a reformatted
C                 copy of the table to OUTFIL as IM table TABVER and set
C                 IRET to 0 or issue one or more error messages and set
C                 IRET to a non-zero value if this is not possible.
C                 If the input file does not have an interferometer
C                 model table with version number TABVER then set IRET
C                 to 0.
C
                  CALL FIXPIM (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 90
               END IF
            ELSE IF (TABTYP .EQ. 'MC') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
  100          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a model components table with
C                 version number TABVER than attach a reformatted copy
C                 of the table to OUTFIL as MC table TABVER and set IRET
C                 to 0 or issue one or more error messages and set IRET
C                 to a non-zero value if this is not possible.
C                 If the input file does not have a model components
C                 table with version number TABVER then set IRET to 0.
C
                  CALL FIXPMC (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 100
               END IF
            ELSE IF (TABTYP .EQ. 'NX') THEN
C
C              If INFILE has an index table with version number 1 that
C              can be copied to OUTFIL as NX table 1 then copy it and
C              set IRET to zero.
C              If INFILE has an index table woth version number 1 that
C              can not be copied to OUTFIL then issue one or more error
C              messages and set IRET to a non-zero value.
C              If INFILE does not have an index table with version
C              number 1 then set IRET to 0
C
               CALL COPNDX (INFILE, OUTFIL, IRET)
C
            ELSE IF (TABTYP .EQ. 'OB') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
  110          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has an orbit table with version
C                 number TABVER than attach a reformatted copy of the
C                 table to OUTFIL as OB table TABVER and set IRET to 0
C                 or issue one or more error messages and set IRET to a
C                 non-zero value if this is not possible.
C                 If the input file does not have an orbit table with
C                 version number TABVER then set IRET to 0.
C
                  CALL FIXPOB (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 110
               END IF
            ELSE IF (TABTYP .EQ. 'PC') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
  120          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a phase cal table with version
C                 number TABVER than attach a reformatted copy of the
C                 table to OUTFIL as PC table TABVER and set IRET to 0
C                 or issue one or more error messages and set IRET to a
C                 non-zero value if this is not possible.
C                 If the input file does not have a phase cal table with
C                 version number TABVER then set IRET to 0.
C
                  CALL FIXPPC (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 120
               END IF
            ELSE IF (TABTYP .EQ. 'SU') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
  130          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a source table with version
C                 number TABVER than attach a reformatted copy of the
C                 table to OUTFIL as SU table TABVER and set IRET to 0
C                 or issue one or more error messages and set IRET to a
C                 non-zero value if this is not possible.
C                 If the input file does not have a source table with
C                 version number TABVER then set IRET to 0.
C
                  CALL FIXPSU (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 130
               END IF
            ELSE IF (TABTYP .EQ. 'TY') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
  140          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a system temperature table with
C                 version number TABVER than attach a reformatted copy
C                 of the table to OUTFIL as TY table TABVER and set IRET
C                 to 0 or issue one or more error messages and set IRET
C                 to a non-zero value if this is not possible.
C                 If the input file does not have a system temperature
C                 table with version number TABVER then set IRET to 0.
C
                  CALL FIXPTY (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 140
               END IF
            ELSE IF (TABTYP .EQ. 'VT') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
  150          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a tape statistics table with
C                 version number TABVER than attach a reformatted copy
C                 of the table to OUTFIL as VT table TABVER and set IRET
C                 to 0 or issue one or more error messages and set IRET
C                 to a non-zero value if this is not possible.
C                 If the input file does not have a tape statistics
C                 table with version number TABVER then set IRET to 0.
C
                  CALL FIXPVT (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 150
               END IF
C
            ELSE IF (TABTYP .EQ. 'WX') THEN
C
C              Process version numbers in reverse order until all table
C              versions have been processed or an error is detected:
C
  160          IF ((IRET .EQ. 0) .AND. (TABVER .GT. 0)) THEN
C
C                 If the input file has a weather table with version
C                 number TABVER than attach a reformatted copy of the
C                 table to OUTFIL as WX table TABVER and set IRET to 0
C                 or issue one or more error messages and set IRET to a
C                 non-zero value if this is not possible.
C                 If the input file does not have a tape statistics
C                 table with version number TABVER then set IRET to 0.
C
                  CALL FIXPWX (INFILE, OUTFIL, TABVER, IRET)
C
                  TABVER = TABVER - 1
               GO TO 160
               END IF
            END IF
         GO TO 10
         END IF
C
      ELSE
         WRITE (MSGTXT, 9160) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9160 FORMAT ('FXTABS: FAILED TO READ CATALOGUE BLOCK (ERROR ', I4, ')')
      END
      SUBROUTINE FXHIST (INPUTS, INFILE, OUTFIL, NPARM, AVNAME, IRET)
C-----------------------------------------------------------------------
C   Copy the history file from INFILE to OUTFIL and update it.
C
C   Inputs:
C     INPUTS   C*(*)       name of INPUTS object holding adverb values
C     INFILE   C*(*)       name of UVDATA object used to access input
C                          file
C     OUTFIL   C*(*)       name of UVDATA object used to access output
C                          file
C     NPARM    I           number of adverbs
C     AVNAME   C(NPARM)*8  names of adverbs
C
C   Output:
C     IRET     I           return status: 0 if history copied and
C                                         updated, non-zero otherwise
C
C   Preconditions:
C      The INPUTS object is initialized and holds the values for the
C      adverbs listed in AVNAME(1:NPARM).
C      INFILE exists.
C      OUTFIL exists.
C
C   Postconditions:
C      If IRET is zero then the history has been copied to the output
C      file and updated.
C      If IRET is not zero then at least one error message has been
C      issued.
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*)
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   NPARM
      CHARACTER AVNAME(NPARM)*8
      INTEGER   IRET
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL OHCOPY (INFILE, OUTFIL, IRET)
      IF (IRET .EQ. 0) THEN
C
C        History was copied successfully.
C
C        Add task settings to history, if possible:
C
         CALL OHLIST (INPUTS, AVNAME, NPARM, OUTFIL, IRET)
         IF (IRET .EQ. 0) THEN
C
C           History was updated successfully.
C
C           Add mapping summary, if possible:
C
            CALL FXHMAP (INFILE, OUTFIL, IRET)
C
         ELSE
C
C           Failed to write adverb settings:
C
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Failed to copy history records.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXHIST: FAILED TO WRITE ADVERBS TO HISTORY (ERROR ', I4,
     *        ')')
 9001 FORMAT ('FXHIST: FAILED TO COPY HISTORY FILE (ERROR ', I4, ')')
      END
      SUBROUTINE PARENS (REPEAT, STR, LPAR, IRET)
C-----------------------------------------------------------------------
C   Handle a parenthesized polarization sequence in the polarization
C   string and check for invalid characters.
C
C   Intended function:
C
C   [ REPEAT > 0 ->
C        [ STR begins with a sequence of characters from {'R', 'L', 'X',
C          'Y'} that contains at least one character and is followed by
C          a closing parenthesis ->
C              POLSTR, LPAR, REPEAT, IRET :=
C                 POLSTR + REPEAT copies of the characters preceding the
C                    closing parenthesis,
C                 index of closing parenthesis in STR, any, 0
C        | else
C              POLSTR, LPAR, REPEAT, message list, IRET :=
C                 any, any, any, message list + syntax error message,
C                 non-zero ]
C   ]
C
C   Input:
C      STR     C*(*) input string that begins with a sequence of
C                    characters from {'R', 'L', 'X', 'Y'} that contains
C                    at least one character and is followed by a closing
C                    parenthesis
C
C   Input/output:
C      REPEAT  I     repeat count (destroyed)
C
C   Outputs:
C      LPAR    I     index of closing parenthesis
C      IRET    I     return status: 0 if sequence processed,
C                                   non-zero otherwise
C
C   Preconditions:
C      REPEAT > 0
C
C   Postconditions:
C      If IRET is zero then REPEAT copies of the sequence of characters
C      preceding the closing parenthesis have been appended to POLSTR.
C      If IRET is not zero then at least one error message has been
C      issued.
C-----------------------------------------------------------------------
      INTEGER   REPEAT
      CHARACTER STR*(*)
      INTEGER   LPAR
      INTEGER   IRET
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C
C     Find the end of the sequence of characters:
C
      LPAR = 1
C
C     Invariant: STR(1:LPAR-1) in {'R', 'L', 'X', 'Y'}
C     Bound: LEN(STR) - LPAR
C
   10 IF ((LPAR .LE. LEN (STR))
     *    .AND. ((STR(LPAR:LPAR) .EQ. 'R')
     *           .OR. (STR(LPAR:LPAR) .EQ. 'L')
     *           .OR. (STR(LPAR:LPAR) .EQ. 'X')
     *           .OR. (STR(LPAR:LPAR) .EQ. 'Y'))) THEN
         LPAR = LPAR + 1
      GO TO 10
      END IF
C
      IF ((1 .LT. LPAR) .AND. (LPAR .LE. LEN (STR))) THEN
         IF (STR(LPAR:LPAR) .EQ. ')') THEN
C
C           The parenthesized group is valid.
C
C           Update POLSTR:
C
            CALL APDPOL (REPEAT, STR(1:LPAR - 1))
C
         ELSE
C
C           There is an invalid character in STR.
C
            WRITE (MSGTXT, 9010)
            CALL MSGWRT (9)
            WRITE (MSGTXT, 9011) STR(LPAR:LPAR), LPAR
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE IF (LPAR .EQ. 1) THEN
C
C        There were no characters preceding the closing parenthesis.
C
         WRITE (MSGTXT, 9010)
         CALL MSGWRT (9)
         MSGTXT = 'PARENTHESES MUST CONTAIN AT LEAST ONE CHARACTER'
         CALL MSGWRT (9)
         IRET = 1
      ELSE
C
C        There was no closing parenthesis.
C
         WRITE (MSGTXT, 9010)
         CALL MSGWRT (9)
         MSGTXT = 'MISSING A CLOSING PARENTHESIS'
         CALL MSGWRT (9)
         IRET = 1
      END IF
C-----------------------------------------------------------------------
 9010 FORMAT ('PARENS: POLARIZATION STRING ERROR')
 9011 FORMAT ('PARENS: CHARACTER ', A1, ' ILLEGAL AT POSITION ', I2)
      END
      SUBROUTINE APDPOL (REPEAT, STR)
C-----------------------------------------------------------------------
C   Append REPEAT copies of STR to POLSTR.
C
C   Inputs:
C     STR      C*(*) string
C
C   Input/output:
C     REPEAT   I     repeat count (trashed on exit)
C
C   Preconditions:
C      REPEAT > 0
C
C   Postconditions:
C      POLSTR has been constructed by appending REPEAT copies of STR to
C      the non-blank prefix of POLSTR and trimming the result to a
C      maximum of MAXIF characters.
C-----------------------------------------------------------------------
      INTEGER   REPEAT
      CHARACTER STR*(*)
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
   10 IF ((REPEAT .NE. 0) .AND. (NSTK .NE. MAXIF)) THEN
         POLSTR = POLSTR(1:NSTK) // STR
         NSTK   = ITRIM (POLSTR)
         REPEAT = REPEAT - 1
      GO TO 10
      END IF
      END
      SUBROUTINE FXLKUP (NDIM, STKAXI, IFAXIS, INAXIS, OUTAXI, INLEN,
     *                   LOOKUP)
C-----------------------------------------------------------------------
C   Build a lookup table from input data array indices to output data
C   array indices.
C
C   Intended Function:
C
C     [ 1 <= NDIM <= 7 and STKAXI is number of STOKES axis
C       and IFAXIS is number of IF axis
C       and INAXIS(1:NDIM) is list of axis dimensions of input array
C       and OUTAXI(1:NDIM) is list of axis dimensions of output array
C       and INLEN is number of elements in input data array ->
C           LOOKUP(1:INLEN) := linear output array index for each
C                              linear input array index ]
C
C   Inputs:
C     NDIM     I        number of data array dimensions
C     STKAXI   I        axis number of STOKES axis
C     IFAXIS   I        axis number of IF axis
C     INAXIS   I(NDIM)  length of each input array axis
C     OUTAXI   I(NDIM)  length of each output array axis
C     INLEN    I        number of elements in input data array
C
C   Output:
C     LOOKUP   I(INLEN) output index lookup table
C
C   Postconditions:
C     LOOKUP maps each index in the input data array to an index in the
C     output data array
C-----------------------------------------------------------------------
      INTEGER   NDIM
      INTEGER   STKAXI
      INTEGER   IFAXIS
      INTEGER   INAXIS(NDIM)
      INTEGER   OUTAXI(NDIM)
      INTEGER   INLEN
      INTEGER   LOOKUP(INLEN)
C
C     Local variables:
C
C     ININC    index increments for each axis of the input data array
C     OUTINC   index increments for each axis of the output data array
C     AXIS     axis number
C     IDX      multidimensional index vector (zero-based)
C     POINT    linear index
C     TEMP     Temporary storage
C
      INTEGER   ININC(7)
      INTEGER   OUTINC(7)
      INTEGER   AXIS
      INTEGER   IDX(7)
      INTEGER   POINT
      INTEGER   TEMP
C
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C     The strategy is to unpack each linear index to a multidimensional
C     index, transform the STOKES and IF coordinates, and then translate
C     the multidimensional index back into a linear index. Zero-based
C     indices are used to simplify the arithmetic.
C
C     Build tables of increments for each axis in the input and output
C     data:
C
      ININC(1)  = 1
      OUTINC(1) = 1
C
C     Invariant: for all i in the range 2 to AXIS, ININC(i) is the
C                product of INAXIS(1:i-1) and OUTINC(i) is the product
C                of OUTAXI(1:i-1)
C     Bound: NDIM - AXIS
C
      DO 10 AXIS = 1, NDIM - 1
         ININC(AXIS + 1)  = ININC(AXIS) * INAXIS(AXIS)
         OUTINC(AXIS + 1) = OUTINC(AXIS) * OUTAXI(AXIS)
   10 CONTINUE
C
C     Invariant: for all i in the range 1 to POINT - 1, LOOKUP(i) is
C                the output index corresponding to input index i
C     Bound: INLEN - POINT + 1
C
      DO 40 POINT = 1, INLEN
C
C        Set IDX(1:NDIM) to the multidimensional, zero-based index
C        corresponding to one-dimensional index POINT
C
         TEMP = POINT - 1
         DO 20 AXIS = NDIM, 1, -1
            IDX(AXIS) = TEMP / ININC(AXIS)
            TEMP      = MOD (TEMP, ININC(AXIS))
   20    CONTINUE
C
C        Transform STOKES and IF indices
C
         IDX(STKAXI) = STKIDX(IDX(IFAXIS) + 1) - 1
         IDX(IFAXIS) = IFINDX(IDX(IFAXIS) + 1) - 1
C
C        Calculate output index
C
         LOOKUP(POINT) = 1
         DO 30 AXIS = 1, NDIM
            LOOKUP(POINT) = LOOKUP(POINT) + IDX(AXIS) * OUTINC(AXIS)
   30    CONTINUE
C
   40 CONTINUE
C
      END
      SUBROUTINE FXDAT2 (NREC, INLEN, LOOKUP, INFILE, INDATA, OUTLEN,
     *                   OUTFIL, OUTDAT, IRET)
C-----------------------------------------------------------------------
C   Copy NREC data records from INFILE to OUTFIL.
C
C   Inputs:
C     INLEN    I           length of input data matrix
C     LOOKUP   I(INLEN)    output indices corresponding to input data
C                          indices
C     INFILE   C*(*)       name of UVDATA object used to access input
C                          file
C     OUTLEN   I           length of output data matrix
C     OUTFIL   C*(*)       name of UVDATA object used to access output
C                          file
C
C   Input/output:
C     NREC     I           number of records still to be transferred
C     INDATA   R(INLEN)    input data matrix buffer
C     OUTDAT   R(OUTLEN)   output data matrix buffer
C
C   Output:
C     IRET     I           return status: 0 if all records copied
C                                         non-zero otherwise
C
C   Preconditions:
C     NREC > 0
C     INFILE is open for "raw" reading
C     OUTFIL is open for writing
C
C   Postconditions:
C     If IRET is zero the OUTFIL contains the transformed equivalents of
C     the first NREC records in INFILE and NREC has been set to zero.
C     If IRET is not zero then at least one error message has been
C     issued.
C-----------------------------------------------------------------------
      INTEGER   NREC
      INTEGER   INLEN
      INTEGER   LOOKUP(INLEN)
      CHARACTER INFILE*(*)
      REAL      INDATA(INLEN)
      INTEGER   OUTLEN
      CHARACTER OUTFIL*(*)
      REAL      OUTDAT(OUTLEN)
      INTEGER   IRET
C
C     Local Variables:
C
C     MAXRP    maximumum number of random parameters in an AIPS file
C     RPARM    random parameter array
C     I        array index
C
      INTEGER   MAXRP
      PARAMETER (MAXRP = 14)
      REAL      RPARM(MAXRP)
      INTEGER   I
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C
C     Initialize OUTDAT with blanks with zero weight:
C
C     Only those entries corresponding to data in the input file will be
C     over-written.
C
      DO 10 I = 1, OUTLEN
         IF (MOD (I, 3) .EQ. 0) THEN
            OUTDAT(I) = 0.0
         ELSE
            OUTDAT(I) = FBLANK
         END IF
   10 CONTINUE
C
   20 IF ((IRET .EQ. 0) .AND. (NREC .NE. 0)) THEN
C
C        Read the next record into INDATA, if possible:
C
         CALL UVREAD (INFILE, RPARM, INDATA, IRET)
         IF (IRET .EQ. 0) THEN
C
C           INDATA contains the next input record:
C
            DO 30 I = 1, INLEN
               OUTDAT(LOOKUP(I)) = INDATA(I)
   30       CONTINUE
C
C           OUTDAT contains the next output record:
C
            CALL UVWRIT (OUTFIL, RPARM, OUTDAT, IRET)
            IF (IRET .NE. 0) THEN
C
C              Failed to write data.
C
               WRITE (MSGTXT, 9030) IRET
               CALL MSGWRT (9)
            END IF
         ELSE
C
C           Failed to read data.
C
            WRITE (MSGTXT, 9031) IRET
            CALL MSGWRT (9)
         END IF
C
C        Update number of records remaining:
C
         NREC = NREC - 1
      GO TO 20
      END IF
C-----------------------------------------------------------------------
 9030 FORMAT ('FXDAT2: FAILED TO READ DATA RECORD (ERROR ', I4, ')')
 9031 FORMAT ('FXDAT2: FAILED TO WRITE DATA RECORD (ERROR ', I4, ')')
      END

      SUBROUTINE COPNDX (INFILE, OUTFIL, IRET)
C-----------------------------------------------------------------------
C   Copy index file from INFILE to OUTFIL.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output file
C
C   Output:
C     IRET     I        return status: 0 if index table copied,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   IRET
C
C     Local Variables:
C
C     INTABL   input table name
C     OUTTBL   output table name
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL UV2TAB (INFILE, INTABL, 'NX', 1, IRET)
      IF (IRET .EQ. 0) THEN
C
C        INTABL represents the input file's NX table.
C
         CALL UV2TAB (OUTFIL, OUTTBL, 'NX', 1, IRET)
         IF (IRET .EQ. 0) THEN
C
C           OUTTBL represents the output file's NX table
C
            CALL TBLCOP (INTABL, OUTTBL, IRET)
C
            IF (IRET .NE. 0) THEN
C
C              Failed to copy table.
C
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
C
            CALL TABDES (OUTTBL, IRET1)
            IF (IRET1 .NE. 0) THEN
C
C              Failed to free OUTTBL object.
C
               WRITE (MSGTXT, 9001) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
         ELSE
C
C           Failed to allocate OUTTBL object.
C
            WRITE (MSGTXT, 9002) IRET
            CALL MSGWRT (9)
         END IF
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
C
C           Failed to free INTABL object.
C
            WRITE (MSGTXT, 9003) IRET1
            CALL MSGWRT (9)
            IRET = IRET1
         END IF
C
      ELSE
C
C        Failed to allocate INTABL object.
C
         WRITE (MSGTXT, 9004) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('COPNDX: FAILED TO COPY INDEX TABLE (ERROR ', I4, ')')
 9001 FORMAT ('COPNDX: FAILED TO DESTROY OUTPUT INDEX TABLE OBJECT (',
     *        'ERROR ', I4, ')')
 9002 FORMAT ('COPNDX: FAILED TO CREATE OUTPUT INDEX TABLE OBJECT (',
     *        'ERROR ', I4, ')')
 9003 FORMAT ('COPNDX: FAILED TO DESTROY INPUT INDEX TABLE OBJECT (',
     *        'ERROR ', I4, ')')
 9004 FORMAT ('COPNDX: FAILED TO CREATE INPUT INDEX TABLE OBJECT (',
     *        'ERROR ', I4, ')')
      END
      SUBROUTINE FXHMAP (INFILE, OUTFIL, IRET)
C-----------------------------------------------------------------------
C   Write a summary of the polarization/IF mapping to the output file's
C   history.
C
C   Inputs:
C      INFILE  C*(*) name of UVDATA object used to access input file
C      OUTFIL  C*(*) name of UVDATA object used to access output file
C
C   Output:
C      IRET    I     return status: 0 if history updated
C                                   non-zero otherwise
C
C   Preconditions:
C      INFILE exists
C      OUTFIL exists
C      The polarization and IF mapping has been established
C
C   Postconditions:
C      If IRET is zero then the mapping has been written to the output
C      file history.
C      If IRET is not zero then at least one error message has been
C      issued.
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   IRET
C
C     Local Variables:
C
C     NAXIS    input data matrix dimensions
C     IFAXIS   IF axis number
C     BAND     IF number
C     LINE     line buffer
C
C     TYPE     attribute type
C     DIM      attribute array dimensions
C     CDUMMY   dummy character argument
C
      INTEGER   NAXIS(7)
      INTEGER   IFAXIS
      INTEGER   BAND
      CHARACTER LINE*72
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
      CALL UVDGET (INFILE, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IRET)
      CALL UVDFND (INFILE, 2, 'IF', IFAXIS, IRET)
      IF (IRET .EQ. 0) THEN
C
C        IFAXIS is the index of the IF axis in NAXIS.
C
         LINE = '/ IFs were mapped as follows'
         CALL OHWRIT (LINE, OUTFIL, IRET)
C
         BAND = 0
C
C        Invariant: IRET = 0 implies that the mappings for bands 1 to
C                   BAND have been written to the history file
C        Bound: NAXIS(IFAXIS) - BAND
C
   10    IF ((IRET .EQ. 0) .AND. (BAND .NE. NAXIS(IFAXIS))) THEN
            BAND = BAND + 1
            WRITE (LINE, 1010) BAND, IFINDX(BAND), POLSTR(BAND:BAND)
            CALL OHWRIT (LINE, OUTFIL, IRET)
         GO TO 10
         END IF
C
         IF (IRET .NE. 0) THEN
C
C           A history record was not written successfully.
C
            WRITE (MSGTXT, 9010) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
         MSGTXT = 'FXHMAP: CORRUPT INPUT FILE HAS NO IF AXIS'
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 1010 FORMAT ('/   ', I4, ' -> ', I4, A1)
 9010 FORMAT ('FXHMAP: FAILED TO UPDATE HISTORY FILE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPAN (INFILE, OUTFIL, ANVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy antenna table ANVER.
C
C   If there is an AN table with version number ANVER attached to INFILE
C   then attach a transformed copy to OUTFIL with version number ANVER
C   and set IRET to zero. If there is no AN table with version number
C   ANVER attached to INFILE then set IRET to zero. If the table can not
C   be transformed or copied then issue a fatal error message and set
C   IRET to a non-zero value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     ANVER    I        antenna table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   ANVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input AN table
C              (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'AN', ANVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as AN
C           table version number ANVER and set IRET to zero or issue one
C           or more fatal error messages and set IRET to a non-zero
C           value:
C
            CALL FXPAN2 (INTABL, OUTFIL, ANVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPAN: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPAN: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPAN2 (INTABL, OUTFIL, ANVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   ANVER.
C
C   Either attach a copy of the antenna table INTABL with polarization
C   labels modified to be consistent with the current IF/polarization
C   mapping to OUTFIL as antenna table version number ANVER and set IRET
C   to zero or issue one or more fatal error messages and set IRET to a
C   non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input AN
C                       table: must reference an existing AN table and
C                       must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     ANVER    I        AN table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   ANVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C     EXISTS   does the table corresponding to OUTTBL already exist?
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'AN', ANVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If there is already a table corresponding to OUTTBL then either
C        remove it and set IRET to zero or issue one or more fatal error
C        messages and set IRET to a non-zero value. If there is no table
C        corresponding to OUTTBL then set IRET to 0.
C
         CALL TABEXI (OUTTBL, EXISTS, IRET)
         IRET = 0
         IF (EXISTS) THEN
            CALL TABRMV (OUTTBL, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Announce table being copied:
C
            WRITE (MSGTXT, 4000) ANVER
            CALL MSGWRT (4)
C
C           Either make OUTTBL a transformed copy of INTABL and set IRET
C           to zero or issue one or more fatal error messages and set
C           IRET to a non-zero value:
C
            CALL FXPAN3 (INTABL, OUTTBL, IRET)
C
         END IF
C
C        Either delete the OUTTBL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (OUTTBL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9001) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 4000 FORMAT ('Fixing AN table version ', I4)
 9000 FORMAT ('FXPAN2: TABLE REMOVAL FAILURE (ERROR ', I4, ')')
 9001 FORMAT ('FXPAN2: OBJECT DESTRUCTION FAILURE (ERROR ', I4, ')')
 9002 FORMAT ('FXPAN2: OBJECT ALLOCATION FAILURE (ERROR ', I4, ')')
      END
      SUBROUTINE FXPAN3 (INTABL, OUTTBL, IRET)
C-----------------------------------------------------------------------
C   Make AN table OUTTBL a transformed copy of INTABL.
C
C   Either create OUTTBL as a copy of INTABL with the polarization
C   labels changed to be consistent with the current IF/polarization
C   mapping and set IRET to 0 or issue one or more fatal error messages
C   and set IRET to a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of table object used to access input AN
C                       table: must reference an existing AN table
C                       belonging to a file for which the
C                       IF/polarization mapping is valid
C     OUTTBL   C*(*)    name of table object used to access output AN
C                       table: must not reference an existing table
C
C   Output:
C     IRET     I        return status: 0 if output table created,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTTBL*(*)
      INTEGER   IRET
C
C     Local variables:
C
C     NEWTYA   new polarization type for feed A
C     NEWTYB   new polarization type for feed B
C
C     NUMROW   number of rows in INTABL
C
C     ANROW    AN table row number
C     ARRAYC   coordinates of array centre
C     GSTIA0   GST at midnight
C     DEGPDY   earth rotation rate
C     SAFREQ   subarray frequency
C     RDATE    subarray reference date
C     POLRXY   coordinates of north pole
C     UT1UTC   UT1 offset from UTC
C     DATUTC   data time offset from UTC
C     TIMSYS   time system name
C     ANAME    array name
C     NUMORB   number of orbital parameters
C     NOPCAL   number of polarization calibration parameters
C     ANFQID   array frequency ID
C
C     ROW      number of last row read from INTABL
C     OUTROW   number of next row to write to OUTTBL
C
C     MAXORB   maximum number of orbital parameters
C     MAXPCL   maximum number of polarization calibration parameters
C     ANNAME   antenna name
C     STAXYZ   antenna coordinates
C     ORBPRM   orbital parameters
C     NOSTA    antenna number
C     MNTSTA   mount type code
C     STAXOF   axis offset
C     POLTYA   feed A polarization type
C     POLAA    feed A position angle
C     POLCA    feed A polarization calibration parameters
C     POLTYB   feed B polarization type
C     POLAB    feed B position angle
C     POLCB    feed B polarization calibration parameters
C
C     TYPE     attribute type code
C     DIM      attribute dimensions list
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      CHARACTER NEWTYA*2
      CHARACTER NEWTYB*2
C
      INTEGER   NUMROW
C
      INTEGER          ANROW
      DOUBLE PRECISION ARRAYC(3)
      DOUBLE PRECISION GSTIA0
      DOUBLE PRECISION DEGPDY
      DOUBLE PRECISION SAFREQ
      CHARACTER        RDATE*8
      REAL             POLRXY(2)
      REAL             UT1UTC
      REAL             DATUTC
      CHARACTER        TIMSYS*8
      CHARACTER        ANAME*8, XYZHAN*8, TFRAME*8
      INTEGER          NUMORB
      INTEGER          NOPCAL, ANTNIF
      INTEGER          ANFQID
C
      INTEGER   ROW
      INTEGER   OUTROW
C
      INTEGER          MAXORB
      PARAMETER        (MAXORB = 12)
      CHARACTER        ANNAME*8
      DOUBLE PRECISION STAXYZ(3)
      DOUBLE PRECISION ORBPRM(MAXORB)
      INTEGER          NOSTA
      INTEGER          MNTSTA
      REAL             STAXOF
      REAL             DIAMAN
      CHARACTER        POLTYA*2
      REAL             POLAA
      CHARACTER        POLTYB*2
      REAL             POLAB
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1, ITEMP, NEWNIF
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
      REAL             FWHMAN(MAXIF)
      REAL             POLCA(2*MAXIF)
      REAL             POLCB(2*MAXIF)
C-----------------------------------------------------------------------
C                                       max IF number out
      NEWNIF = 1
      DO 5 ITEMP = 1,MAXIF
         NEWNIF = MAX (NEWNIF, IFINDX(ITEMP))
 5       CONTINUE
C
C     Set new feed labels according to the current polarization mapping:
C
      IF (STK1 .EQ. -1) THEN
         NEWTYA = 'R '
         IF (STKDIM .EQ. 1) THEN
            NEWTYB = '  '
         ELSE
            NEWTYB = 'L '
         END IF
      ELSE IF (STK1 .EQ. -2) THEN
         NEWTYA = 'L '
         NEWTYB = '  '
      ELSE IF (STK1 .EQ. -5) THEN
         NEWTYA = 'X '
         IF (STKDIM .EQ. 1) THEN
            NEWTYB = '  '
         ELSE
            NEWTYB = 'Y '
         END IF
      ELSE
         NEWTYA = 'Y '
         NEWTYB = '  '
      END IF
C-----------------------------------------------------------------------
C
C     A deficiency in the TABLE interface prevents the number of rows
C     being read if the TABLE has not been open with the generic TABOPN
C     interface so the table must opened to read the number of rows and
C     then closed before being opened by the specialized OANINI
C     interface.
C
C     If INTABL can be opened and closed then set NUMROW to the number
C     of rows in INTABL and IRET to zero otherwise issue one or more
C     fatal error messages and set IRET to a non-zero value:
C
      CALL TABOPN (INTABL, 'READ', IRET)
      IF (IRET .EQ. 0) THEN
C
C        Set NUMROW to the number of rows in INTABL and IRET to 0:
C
         CALL TABGET (INTABL, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
         CALL TABCLO (INTABL, IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Table open failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
      IF (IRET .EQ. 0) THEN
C
C        Either make OUTTBL a copy of INTABL with polarization type A
C        set to NEWTYA and polarization type B set to NEWTYB for each
C        antenna and set IRET to 0 or issue one or more fatal error
C        messages and set IRET to a non-zero value:
C
         CALL OANINI (INTABL, 'READ', ANROW, ARRAYC, GSTIA0, DEGPDY,
     *      SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *      XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
         IF (IRET .EQ. 0) THEN
C
C           Input table was opened for reading.
C
            CALL OANINI (OUTTBL, 'WRIT', ANROW, ARRAYC, GSTIA0, DEGPDY,
     *         SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *         XYZHAN, TFRAME, NUMORB, NOPCAL, NEWNIF, ANFQID, IRET)
            IF (IRET .EQ. 0) THEN
C
C              Output table was opened for writing.
C
               ROW    = 0
               OUTROW = 1
   10          IF ((IRET .EQ. 0) .AND. (ROW .NE. NUMROW)) THEN
                  ROW   = ROW + 1
                  ANROW = ROW
                  CALL OTABAN (INTABL, 'READ', ANROW, ANNAME, STAXYZ,
     *               ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *               POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    Input table row number ROW was read and is not
C                    flagged.
C
C                    Either write output table row number OUTROW,
C                    increment OUTROW, and set IRET to 0 or issue one or
C                    more fatal error messages and set IRET to a
C                    non-zero value:
C
                     CALL OTABAN (OUTTBL, 'WRIT', OUTROW, ANNAME,
     *                  STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *                  FWHMAN, NEWTYA, POLAA, POLCA, NEWTYB, POLAB,
     *                  POLCB, IRET)
C
                     IF (IRET .NE. 0) THEN
C
C                       Failed to write row.
C
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (9)
                     END IF
                  ELSE IF (IRET .LT. 0) THEN
C
C                    Input table row number ROW was read and is flagged.
C
C                    Clear error indicator:
C
                     IRET = 0
C
                  ELSE
C
C                    Failed to read row.
C
                     WRITE (MSGTXT, 9011) IRET
                     CALL MSGWRT (9)
                  END IF
               GO TO 10
               END IF
C
C              Either close the output table and leave IRET unchanged or
C              issue one or more fatal error messages and set IRET to a
C              non-zero value:
C
               CALL OTABAN (OUTTBL, 'CLOS', OUTROW, ANNAME, STAXYZ,
     *            ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, NEWTYA,
     *            POLAA, POLCA, NEWTYB, POLAB, POLCB, IRET1)
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 9012) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
C
            ELSE
C
C              Failed to open output table.
C
               WRITE (MSGTXT, 9013) IRET
               CALL MSGWRT (9)
            END IF
C
C           Either close the input table and leave IRET unchanged or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL OTABAN (INTABL, 'CLOS', ANROW, ANNAME, STAXYZ, ORBPRM,
     *         NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA,
     *         POLCA, POLTYB, POLAB, POLCB, IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           Failed to open input table.
C
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPAN3: FAILED TO CLOSE INPUT TABLE (ERROR ', I4, ')')
 9001 FORMAT ('FXPAN3: FAILED TO OPEN INPUT TABLE (ERROR ', I4, ')')
 9010 FORMAT ('FXPAN3: FAILED TO WRITE TO OUTPUT TABLE (ERROR ', I4,
     *        ')')
 9011 FORMAT ('FXPAN3: FAILED TO READ FROM INPUT TABLE (ERROR ', I4,
     *        ')')
 9012 FORMAT ('FXPAN3: FAILED TO CLOSE OUTPUT TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXPAN3: FAILED TO OPEN OUTPUT TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPFQ (INFILE, OUTFIL, FQVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy frequency table FQVER.
C
C   If there is an FQ table with version number FQVER attached to INFILE
C   then attach a transformed copy to OUTFIL with version number FQVER
C   and set IRET to zero. If there is no FQ table with version number
C   FQVER attached to INFILE then set IRET to zero. If the table can not
C   be transformed or copied then issue a fatal error message and set
C   IRET to a non-zero value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     FQVER    I        frequency table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   FQVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input FQ table
C              (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'FQ', FQVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as FQ
C           table version number FQVER and set IRET to zero or issue one
C           or more fatal error messages and set IRET to a non-zero
C           value:
C
            CALL FXPFQ2 (INTABL, OUTFIL, FQVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPFQ: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPFQ: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPFQ2 (INTABL, OUTFIL, FQVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   FQVER.
C
C   Either attach a copy of the frequency table INTABL with IF entries
C   modified to be consistent with the current IF/polarization mapping
C   to OUTFIL as frequency table version number FQVER and set IRET to
C   zero or issue one or more fatal error messages and set IRET to a
C   non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input FQ
C                       table: must reference an existing FQ table and
C                       must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     FQVER    I        FQ table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   FQVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C     EXISTS   does the table corresponding to OUTTBL already exist?
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'FQ', FQVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If there is already a table corresponding to OUTTBL then either
C        remove it and set IRET to zero or issue one or more fatal error
C        messages and set IRET to a non-zero value. If there is no table
C        corresponding to OUTTBL then set IRET to 0.
C
         CALL TABEXI (OUTTBL, EXISTS, IRET)
         IRET = 0
         IF (EXISTS) THEN
            CALL TABRMV (OUTTBL, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Announce table being copied:
C
            WRITE (MSGTXT, 4000) FQVER
            CALL MSGWRT (4)
C
C           Either make OUTTBL a transformed copy of INTABL and set IRET
C           to zero or issue one or more fatal error messages and set
C           IRET to a non-zero value:
C
            CALL FXPFQ3 (INTABL, OUTTBL, IRET)
C
         END IF
C
C        Either delete the OUTTBL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (OUTTBL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9001) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 4000 FORMAT ('Fixing FQ table version ', I4)
 9000 FORMAT ('FXPFQ2: TABLE REMOVAL FAILURE (ERROR ', I4, ')')
 9001 FORMAT ('FXPFQ2: OBJECT DESTRUCTION FAILURE (ERROR ', I4, ')')
 9002 FORMAT ('FXPFQ2: OBJECT ALLOCATION FAILURE (ERROR ', I4, ')')
      END
      SUBROUTINE FXPFQ3 (INTABL, OUTTBL, IRET)
C-----------------------------------------------------------------------
C   Make FQ table OUTTBL a transformed copy of INTABL.
C
C   Either create OUTTBL as a copy of INTABL with the IF entries
C   re-mapped according to the current IF/polarization mapping and set
C   IRET to 0 or issue one or more fatal error messages and set IRET to
C   a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of table object used to access input FQ
C                       table: must reference an existing FQ table
C                       belonging to a file for which the
C                       IF/polarization mapping is valid
C     OUTTBL   C*(*)    name of table object used to access output FQ
C                       table: must not reference an existing table
C
C   Output:
C     IRET     I        return status: 0 if output table created,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTTBL*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     NUMROW   number of rows in INTABL
C
C     FQROW    FQ table row number
C     NUMIF    number of IFs in INTABL
C
C     ROW      number of last row read from INTABL
C     OUTROW   number of next row to write to OUTTBL
C
C     FQID     frequency ID
C     IFFREQ   list of IF frequencies
C     IFCHW    list of IF channel bandwidths
C     IFTBW    list of IF total bandwidths
C     IFSIDE   list of IF sidebands
C
C     IFNUM    IF number
C
C     NEWFRQ   list of remapped frequencies
C     NEWCHW   list of remapped channel bandwidths
C     NEWTBW   list of remapped total bandwidths
C     NEWSID   list of new sidebands
C
C     TYPE     attribute type code
C     DIM      attribute dimensions list
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   NUMROW
C
      INTEGER   FQROW
      INTEGER   NUMIF
C
      INTEGER   ROW
      INTEGER   OUTROW
C
      INTEGER          FQID
      DOUBLE PRECISION IFFREQ(MAXIF)
      REAL             IFCHW(MAXIF)
      REAL             IFTBW(MAXIF)
      INTEGER          IFSIDE(MAXIF)
      CHARACTER        BNDCOD(MAXIF)*8
C
      INTEGER   IFNUM
C
      DOUBLE PRECISION NEWFRQ(MAXIF)
      REAL             NEWCHW(MAXIF)
      REAL             NEWTBW(MAXIF)
      INTEGER          NEWSID(MAXIF)
      CHARACTER        NEWCOD(MAXIF)*8
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     A deficiency in the TABLE interface prevents the number of rows
C     being read if the TABLE has not been open with the generic TABOPN
C     interface so the table must opened to read the number of rows and
C     then closed before being opened by the specialized OFQINI
C     interface.
C
C     If INTABL can be opened and closed then set NUMROW to the number
C     of rows in INTABL and IRET to zero otherwise issue one or more
C     fatal error messages and set IRET to a non-zero value:
C
      CALL TABOPN (INTABL, 'READ', IRET)
      IF (IRET .EQ. 0) THEN
C
C        Set NUMROW to the number of rows in INTABL and IRET to 0:
C
         CALL TABGET (INTABL, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
         CALL TABCLO (INTABL, IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Table open failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
      IF (IRET .EQ. 0) THEN
C
C        Either make OUTTBL a copy of INTABL with IF entries remapped
C        according to the current polarization/IF mapping and set IRET
C        to 0 or issue one or more fatal error messages and set IRET to
C        a non-zero value:
C
         CALL OFQINI (INTABL, 'READ', FQROW, NUMIF, IRET)
         IF (IRET .EQ. 0) THEN
C
C           Input table was opened for reading.
C
            CALL OFQINI (OUTTBL, 'WRIT', FQROW, IFDIM, IRET)
            IF (IRET .EQ. 0) THEN
C
C              Output table was opened for writing.
C
               ROW    = 0
               OUTROW = 1
   10          IF ((IRET .EQ. 0) .AND. (ROW .NE. NUMROW)) THEN
                  ROW   = ROW + 1
                  FQROW = ROW
                  CALL OTABFQ (INTABL, 'READ', FQROW, NUMIF, FQID,
     *               IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    Input table row number ROW was read and is not
C                    flagged.
C
                     DO 20 IFNUM = 1, NUMIF
                        NEWFRQ(IFINDX(IFNUM)) = IFFREQ(IFNUM)
                        NEWCHW(IFINDX(IFNUM)) = IFCHW(IFNUM)
                        NEWTBW(IFINDX(IFNUM)) = IFTBW(IFNUM)
                        NEWSID(IFINDX(IFNUM)) = IFSIDE(IFNUM)
                        NEWCOD(IFINDX(IFNUM)) = BNDCOD(IFNUM)
   20                   CONTINUE
C
C                    Either write output table row number OUTROW,
C                    increment OUTROW, and set IRET to 0 or issue one or
C                    more fatal error messages and set IRET to a
C                    non-zero value:
C
                     CALL OTABFQ (OUTTBL, 'WRIT', OUTROW, IFDIM, FQID,
     *                  NEWFRQ, NEWCHW, NEWTBW, NEWSID, NEWCOD, IRET)
C
                     IF (IRET .NE. 0) THEN
C
C                       Failed to write row.
C
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (9)
                     END IF
                  ELSE IF (IRET .LT. 0) THEN
C
C                    Input table row number ROW was read and is flagged.
C
C                    Clear error indicator:
C
                     IRET = 0
C
                  ELSE
C
C                    Failed to read row.
C
                     WRITE (MSGTXT, 9011) IRET
                     CALL MSGWRT (9)
                  END IF
               GO TO 10
               END IF
C
C              Either close the output table and leave IRET unchanged or
C              issue one or more fatal error messages and set IRET to a
C              non-zero value:
C
               CALL OTABFQ (OUTTBL, 'CLOS', OUTROW, IFDIM, FQID, NEWFRQ,
     *            NEWCHW, NEWTBW, NEWSID, NEWCOD, IRET1)
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 9012) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
C
            ELSE
C
C              Failed to open output table.
C
               WRITE (MSGTXT, 9013) IRET
               CALL MSGWRT (9)
            END IF
C
C           Either close the input table and leave IRET unchanged or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL OTABFQ (INTABL, 'CLOS', FQROW, NUMIF, FQID, IFFREQ,
     *         IFCHW, IFTBW, IFSIDE, BNDCOD, IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           Failed to open input table.
C
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPFQ3: FAILED TO CLOSE INPUT TABLE (ERROR ', I4, ')')
 9001 FORMAT ('FXPFQ3: FAILED TO OPEN INPUT TABLE (ERROR ', I4, ')')
 9010 FORMAT ('FXPFQ3: FAILED TO WRITE TO OUTPUT TABLE (ERROR ', I4,
     *        ')')
 9011 FORMAT ('FXPFQ3: FAILED TO READ FROM INPUT TABLE (ERROR ', I4,
     *        ')')
 9012 FORMAT ('FXPFQ3: FAILED TO CLOSE OUTPUT TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXPFQ3: FAILED TO OPEN OUTPUT TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPSU (INFILE, OUTFIL, SUVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy source table SUVER.
C
C   If there is an SU table with version number SUVER attached to INFILE
C   then attach a transformed copy to OUTFIL with version number SUVER
C   and set IRET to zero. If there is no SU table with version number
C   SUVER attached to INFILE then set IRET to zero. If the table can not
C   be transformed or copied then issue a fatal error message and set
C   IRET to a non-zero value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     SUVER    I        frequency table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   SUVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input SU table
C              (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'SU', SUVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as SU
C           table version number SUVER and set IRET to zero or issue one
C           or more fatal error messages and set IRET to a non-zero
C           value:
C
            CALL FXPSU2 (INTABL, OUTFIL, SUVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPSU: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPSU: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPSU2 (INTABL, OUTFIL, SUVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   SUVER.
C
C   Either attach a copy of the source table INTABL with IF entries
C   modified to be consistent with the current IF/polarization mapping
C   to OUTFIL as source table version number SUVER and set IRET to zero
C   or issue one or more fatal error messages and set IRET to a non-zero
C   value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input SU
C                       table: must reference an existing SU table and
C                       must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     SUVER    I        SU table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   SUVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C     EXISTS   does the table corresponding to OUTTBL already exist?
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'SU', SUVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If there is already a table corresponding to OUTTBL then either
C        remove it and set IRET to zero or issue one or more fatal error
C        messages and set IRET to a non-zero value. If there is no table
C        corresponding to OUTTBL then set IRET to 0.
C
         CALL TABEXI (OUTTBL, EXISTS, IRET)
         IRET = 0
         IF (EXISTS) THEN
            CALL TABRMV (OUTTBL, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Announce table being copied:
C
            WRITE (MSGTXT, 4000) SUVER
            CALL MSGWRT (4)
C
C           Either make OUTTBL a transformed copy of INTABL and set IRET
C           to zero or issue one or more fatal error messages and set
C           IRET to a non-zero value:
C
            CALL FXPSU3 (INTABL, OUTTBL, IRET)
C
         END IF
C
C        Either delete the OUTTBL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (OUTTBL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9001) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 4000 FORMAT ('Fixing SU table version ', I4)
 9000 FORMAT ('FXPSU2: TABLE REMOVAL FAILURE (ERROR ', I4, ')')
 9001 FORMAT ('FXPSU2: OBJECT DESTRUCTION FAILURE (ERROR ', I4, ')')
 9002 FORMAT ('FXPSU2: OBJECT ALLOCATION FAILURE (ERROR ', I4, ')')
      END
      SUBROUTINE FXPSU3 (INTABL, OUTTBL, IRET)
C-----------------------------------------------------------------------
C   Make SU table OUTTBL a transformed copy of INTABL.
C
C   Either create OUTTBL as a copy of INTABL with the IF entries
C   re-mapped according to the current IF/polarization mapping and set
C   IRET to 0 or issue one or more fatal error messages and set IRET to
C   a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of table object used to access input SU
C                       table: must reference an existing SU table
C                       belonging to a file for which the
C                       IF/polarization mapping is valid
C     OUTTBL   C*(*)    name of table object used to access output SU
C                       table: must not reference an existing table
C
C   Output:
C     IRET     I        return status: 0 if output table created,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTTBL*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     NUMROW   number of rows in INTABL
C
C     SUROW    SU table row number
C     NUMIF    number of IFs in INTABL
C     VELTYP   velocity type
C     VELDEF   velocity definition
C     FREQID   frequency ID
C
C     ROW      number of last row read from INTABL
C     OUTROW   number of next row to write to OUTTBL
C
C     IDSOU    source ID number
C     SOUNAM   source name
C     QUAL     source qualifier
C     CALCOD   cal code
C     FLUX     source fluxes by IF
C     FREQO    source frequency offsets by IF
C     BANDW    source bandwidth
C     RAEPO    source RA at epoch
C     DECEPO   source declination at epoch
C     EPOCH    epoch
C     RAAPP    apparent RA
C     DECAPP   apparent declination
C     LSRVEL   LSR velocity by IF
C     LRESTF   LST rest frequency by IF
C     PMRA     proper motion in RA
C     PMDEC    proper motion in declination
C
C     IFNUM    IF number
C
C     NEWFLX   list of remapped fluxes
C     NEWFRQ   list of remapped frequency offsets
C     NEWVEL   list of remapped velocities
C     NEWRST   list of remapped rest frequencies
C
C     TYPE     attribute type code
C     DIM      attribute dimensions list
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   NUMROW
C
      INTEGER   SUROW
      INTEGER   NUMIF
      CHARACTER VELTYP*8
      CHARACTER VELDEF*8
      INTEGER   FREQID
C
      INTEGER   ROW
      INTEGER   OUTROW
C
      INTEGER          IDSOU
      CHARACTER        SOUNAM*16
      INTEGER          QUAL
      CHARACTER        CALCOD*4
      REAL             FLUX(4, MAXIF)
      DOUBLE PRECISION FREQO(MAXIF)
      DOUBLE PRECISION BANDW
      DOUBLE PRECISION RAEPO
      DOUBLE PRECISION DECEPO
      DOUBLE PRECISION EPOCH
      DOUBLE PRECISION RAAPP, RAOBS
      DOUBLE PRECISION DECAPP, DECOBS
      DOUBLE PRECISION LSRVEL(MAXIF)
      DOUBLE PRECISION LRESTF(MAXIF)
      DOUBLE PRECISION PMRA
      DOUBLE PRECISION PMDEC
C
      INTEGER   IFNUM
C
      REAL             NEWFLX(4, MAXIF)
      DOUBLE PRECISION NEWFRQ(MAXIF)
      DOUBLE PRECISION NEWVEL(MAXIF)
      DOUBLE PRECISION NEWRST(MAXIF)
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     A deficiency in the TABLE interface prevents the number of rows
C     being read if the TABLE has not been open with the generic TABOPN
C     interface so the table must opened to read the number of rows and
C     then closed before being opened by the specialized OSUINI
C     interface.
C
C     If INTABL can be opened and closed then set NUMROW to the number
C     of rows in INTABL and IRET to zero otherwise issue one or more
C     fatal error messages and set IRET to a non-zero value:
C
      CALL TABOPN (INTABL, 'READ', IRET)
      IF (IRET .EQ. 0) THEN
C
C        Set NUMROW to the number of rows in INTABL and IRET to 0:
C
         CALL TABGET (INTABL, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
         CALL TABCLO (INTABL, IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Table open failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
      IF (IRET .EQ. 0) THEN
C
C        Either make OUTTBL a copy of INTABL with IF entries remapped
C        according to the current polarization/IF mapping and set IRET
C        to 0 or issue one or more fatal error messages and set IRET to
C        a non-zero value:
C
         CALL OSUINI (INTABL, 'READ', NUMIF, VELTYP, VELDEF, FREQID,
     *                SUROW, IRET)
         IF (IRET .EQ. 0) THEN
C
C           Input table was opened for reading.
C
            CALL OSUINI (OUTTBL, 'WRIT', IFDIM, VELTYP, VELDEF, FREQID,
     *                   SUROW, IRET)
            IF (IRET .EQ. 0) THEN
C
C              Output table was opened for writing.
C
               ROW    = 0
               OUTROW = 1
   10          IF ((IRET .EQ. 0) .AND. (ROW .NE. NUMROW)) THEN
                  ROW   = ROW + 1
                  SUROW = ROW
                  CALL OTABSU (INTABL, 'READ', SUROW, IDSOU, SOUNAM,
     *               QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *               EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL,
     *               LRESTF, PMRA, PMDEC, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    Input table row number ROW was read and is not
C                    flagged.
C
                     DO 20 IFNUM = 1, NUMIF
                        CALL RCOPY (4, FLUX(1, IFNUM),
     *                     NEWFLX(1, IFINDX(IFNUM)))
                        NEWFRQ(IFINDX(IFNUM)) = FREQO(IFNUM)
                        NEWVEL(IFINDX(IFNUM)) = LSRVEL(IFNUM)
                        NEWRST(IFINDX(IFNUM)) = LRESTF(IFNUM)
   20                CONTINUE
C
C                    Either write output table row number OUTROW,
C                    increment OUTROW, and set IRET to 0 or issue one or
C                    more fatal error messages and set IRET to a
C                    non-zero value:
C
                     CALL OTABSU (OUTTBL, 'WRIT', OUTROW, IDSOU, SOUNAM,
     *                  QUAL, CALCOD, NEWFLX, NEWFRQ, BANDW, RAEPO,
     *                  DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS,
     *                  NEWVEL, NEWRST, PMRA, PMDEC, IRET)
                     IF (IRET .NE. 0) THEN
C
C                       Failed to write row.
C
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (9)
                     END IF
                  ELSE IF (IRET .LT. 0) THEN
C
C                    Input table row number ROW was read and is flagged.
C
C                    Clear error indicator:
C
                     IRET = 0
C
                  ELSE
C
C                    Failed to read row.
C
                     WRITE (MSGTXT, 9011) IRET
                     CALL MSGWRT (9)
                  END IF
               GO TO 10
               END IF
C
C              Either close the output table and leave IRET unchanged or
C              issue one or more fatal error messages and set IRET to a
C              non-zero value:
C
               CALL OTABSU (OUTTBL, 'CLOS', OUTROW, IDSOU, SOUNAM, QUAL,
     *            CALCOD, NEWFLX, NEWFRQ, BANDW, RAEPO, DECEPO, EPOCH,
     *            RAAPP, DECAPP, RAOBS, DECOBS, NEWVEL, NEWRST, PMRA,
     *            PMDEC, IRET1)
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 9012) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
C
            ELSE
C
C              Failed to open output table.
C
               WRITE (MSGTXT, 9013) IRET
               CALL MSGWRT (9)
            END IF
C
C           Either close the input table and leave IRET unchanged or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL OTABSU (INTABL, 'CLOS', SUROW, IDSOU, SOUNAM, QUAL,
     *         CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *         DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC,
     *         IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           Failed to open input table.
C
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPSU3: FAILED TO CLOSE INPUT TABLE (ERROR ', I4, ')')
 9001 FORMAT ('FXPSU3: FAILED TO OPEN INPUT TABLE (ERROR ', I4, ')')
 9010 FORMAT ('FXPSU3: FAILED TO WRITE TO OUTPUT TABLE (ERROR ', I4,
     *        ')')
 9011 FORMAT ('FXPSU3: FAILED TO READ FROM INPUT TABLE (ERROR ', I4,
     *        ')')
 9012 FORMAT ('FXPSU3: FAILED TO CLOSE OUTPUT TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXPSU3: FAILED TO OPEN OUTPUT TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPCL (INFILE, OUTFIL, CLVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy calibration table CLVER.
C
C   If there is an CL table with version number CLVER attached to INFILE
C   then attach a transformed copy to OUTFIL with version number CLVER
C   and set IRET to zero. If there is no CL table with version number
C   CLVER attached to INFILE then set IRET to zero. If the table can not
C   be transformed or copied then issue a fatal error message and set
C   IRET to a non-zero value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     CLVER    I        frequency table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   CLVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input CL table
C              (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'CL', CLVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as CL
C           table version number CLVER and set IRET to zero or issue one
C           or more fatal error messages and set IRET to a non-zero
C           value:
C
            CALL FXPCL2 (INTABL, OUTFIL, CLVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPCL: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPCL: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPCL2 (INTABL, OUTFIL, CLVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   CLVER.
C
C   Either attach a copy of the calibration table INTABL with IF entries
C   modified to be consistent with the current IF/polarization mapping
C   to OUTFIL as calibration table version number CLVER and set IRET to
C   zero or issue one or more fatal error messages and set IRET to a
C   non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input CL
C                       table: must reference an existing CL table and
C                       must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     CLVER    I        CL table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   CLVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C     EXISTS   does the table corresponding to OUTTBL already exist?
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'CL', CLVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If there is already a table corresponding to OUTTBL then either
C        remove it and set IRET to zero or issue one or more fatal error
C        messages and set IRET to a non-zero value. If there is no table
C        corresponding to OUTTBL then set IRET to 0.
C
         CALL TABEXI (OUTTBL, EXISTS, IRET)
         IRET = 0
         IF (EXISTS) THEN
            CALL TABRMV (OUTTBL, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Announce table being copied:
C
            WRITE (MSGTXT, 4000) CLVER
            CALL MSGWRT (4)
C
C           Either make OUTTBL a transformed copy of INTABL and set IRET
C           to zero or issue one or more fatal error messages and set
C           IRET to a non-zero value:
C
            CALL FXPCL3 (INTABL, OUTTBL, IRET)
C
         END IF
C
C        Either delete the OUTTBL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (OUTTBL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9001) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 4000 FORMAT ('Fixing CL table version ', I4)
 9000 FORMAT ('FXPCL2: TABLE REMOVAL FAILURE (ERROR ', I4, ')')
 9001 FORMAT ('FXPCL2: OBJECT DESTRUCTION FAILURE (ERROR ', I4, ')')
 9002 FORMAT ('FXPCL2: OBJECT ALLOCATION FAILURE (ERROR ', I4, ')')
      END
      SUBROUTINE FXPCL3 (INTABL, OUTTBL, IRET)
C-----------------------------------------------------------------------
C   Make CL table OUTTBL a transformed copy of INTABL.
C
C   Either create OUTTBL as a copy of INTABL with the IF entries
C   re-mapped according to the current IF/polarization mapping and set
C   IRET to 0 or issue one or more fatal error messages and set IRET to
C   a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of table object used to access input CL
C                       table: must reference an existing CL table
C                       belonging to a file for which the
C                       IF/polarization mapping is valid
C     OUTTBL   C*(*)    name of table object used to access output CL
C                       table: must not reference an existing table
C
C   Output:
C     IRET     I        return status: 0 if output table created,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTTBL*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     NUMROW   number of rows in INTABL
C
C     CLROW    CL table row number
C     NUMANT   number of antennae covered by INTABL
C     NUMPOL   number of polarizations in INTABL
C     NUMIF    number of IFs in INTABL
C     NTERM    number of polynomial terms in INTABL
C     GMMOD    mean gain modulus for INTABL
C
C     ROW      number of last row read from INTABL
C     OUTROW   number of next row to write to OUTTBL
C
C     MAXPLY   maximum polynomial order
C     TIME     solution time
C     TIMINT   solution time interval
C     SOURID   solution source ID
C     ANTNUM   solution antenna number
C     SUBARR   solution subarray number
C     FREQID   solution frequency ID
C     IFR      solution IFR
C     GEODLY   solution total delay
C     DOPOFF   Doppler offsets by IF
C     ATMOS    atmospheric delays by polarization
C     DATMOS   derivatives of atmospheric delay by polarization
C     MBDELY   multiband delays by polarization
C     CLOCK    clock offsets by polarization and IF
C     DCLOCK   clock drift rates by polarization and IF
C     DISP     dispersive delays by polarization and IF
C     DDISP    dispersive delay drift rates by polarization and IF
C     CREAL    real parts of gain by polarization and IF
C     CIMAG    imaginary parts of gain by polarization and IF
C     DELAY    single-band delays by polarization and IF
C     RATE     delay rates by polarization and IF
C     WEIGHT   solution weights by polarization and IF
C     REFA     reference antennae by polarization and IF
C
C     IFNUM    IF number
C
C     NEWDOP   remapped Doppler offsets
C     NEWCRE   remapped real gains
C     NEWCIM   remapped imaginary gains
C     NEWDLY   remapped delays
C     NEWRAT   remapped rates
C     NEWWT    remapped weights
C     NEWREF   remapped reference antennae
C
C     TYPE     attribute type code
C     DIM      attribute dimensions list
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   NUMROW
C
      INTEGER   CLROW
      INTEGER   NUMANT
      INTEGER   NUMPOL
      INTEGER   NUMIF
      INTEGER   NTERM
      REAL      GMMOD
C
      INTEGER   ROW
      INTEGER   OUTROW
C
      INTEGER          MAXPLY
      PARAMETER        (MAXPLY = 12)
      DOUBLE PRECISION TIME
      REAL             TIMINT
      INTEGER          SOURID
      INTEGER          ANTNUM
      INTEGER          SUBARR
      INTEGER          FREQID
      REAL             IFR
      DOUBLE PRECISION GEODLY(MAXPLY)
      REAL             DOPOFF(MAXIF)
      REAL             ATMOS(2)
      REAL             DATMOS(2)
      REAL             MBDELY(2)
      REAL             CLOCK(2)
      REAL             DCLOCK(2)
      REAL             DISP(2)
      REAL             DDISP(2)
      REAL             CREAL(2, MAXIF)
      REAL             CIMAG(2, MAXIF)
      REAL             DELAY(2, MAXIF)
      REAL             RATE(2, MAXIF)
      REAL             WEIGHT(2, MAXIF)
      INTEGER          REFA(2, MAXIF)
C
      INTEGER   IFNUM
C
      REAL      NEWDOP(MAXIF)
      REAL      NEWCRE(2, MAXIF)
      REAL      NEWCIM(2, MAXIF)
      REAL      NEWDLY(2, MAXIF)
      REAL      NEWRAT(2, MAXIF)
      REAL      NEWWT(2, MAXIF)
      INTEGER   NEWREF(2, MAXIF)
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     Fill the remapped arrays with null values. Those values in the
C     range of the polarization/IF mapping function will be over-ridden
C     by values from the source CL table later in this routine:
C
      CALL RFILL (MAXIF, FBLANK, NEWDOP)
      CALL RFILL (2 * MAXIF, FBLANK, NEWCRE)
      CALL RFILL (2 * MAXIF, FBLANK, NEWCIM)
      CALL RFILL (2 * MAXIF, FBLANK, NEWDLY)
      CALL RFILL (2 * MAXIF, FBLANK, NEWRAT)
      CALL RFILL (2 * MAXIF, -1.0, NEWWT)
      CALL FILL (2 * MAXIF, -1, NEWREF)
C-----------------------------------------------------------------------
C
C     A deficiency in the TABLE interface prevents the number of rows
C     being read if the TABLE has not been open with the generic TABOPN
C     interface so the table must opened to read the number of rows and
C     then closed before being opened by the specialized OCLINI
C     interface.
C
C     If INTABL can be opened and closed then set NUMROW to the number
C     of rows in INTABL and IRET to zero otherwise issue one or more
C     fatal error messages and set IRET to a non-zero value:
C
      CALL TABOPN (INTABL, 'READ', IRET)
      IF (IRET .EQ. 0) THEN
C
C        Set NUMROW to the number of rows in INTABL and IRET to 0:
C
         CALL TABGET (INTABL, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
         CALL TABCLO (INTABL, IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Table open failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
      IF (IRET .EQ. 0) THEN
C
C        Either make OUTTBL a copy of INTABL with IF entries remapped
C        according to the current polarization/IF mapping and set IRET
C        to 0 or issue one or more fatal error messages and set IRET to
C        a non-zero value:
C
         CALL OCLINI (INTABL, 'READ', CLROW, NUMANT, NUMPOL, NUMIF,
     *                NTERM, GMMOD, IRET)
         IF (IRET .EQ. 0) THEN
C
C           Input table was opened for reading.
C
            CALL OCLINI (OUTTBL, 'WRIT', CLROW, NUMANT, STKDIM, IFDIM,
     *                   NTERM, GMMOD, IRET)
            IF (IRET .EQ. 0) THEN
C
C              Output table was opened for writing.
C
               ROW    = 0
               OUTROW = 1
   10          IF ((IRET .EQ. 0) .AND. (ROW .NE. NUMROW)) THEN
                  ROW   = ROW + 1
                  CLROW = ROW
                  CALL OTABCL (INTABL, 'READ', CLROW, NUMPOL, NUMIF,
     *                         TIME, TIMINT, SOURID, ANTNUM, SUBARR,
     *                         FREQID, IFR, GEODLY, DOPOFF, ATMOS,
     *                         DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *                         DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *                         REFA, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    Input table row number ROW was read and is not
C                    flagged.
C
                     DO 20 IFNUM = 1, NUMIF
                        NEWDOP(IFINDX(IFNUM)) = DOPOFF(IFNUM)
                        NEWCRE(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = CREAL(1, IFNUM)
                        NEWCIM(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = CIMAG(1, IFNUM)
                        NEWDLY(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = DELAY(1, IFNUM)
                        NEWRAT(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = RATE(1, IFNUM)
                        NEWWT(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = WEIGHT(1, IFNUM)
                        NEWREF(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = REFA(1, IFNUM)
   20                CONTINUE
                     IF (STKDIM .GT. 1) THEN
                        ATMOS(2)  = ATMOS(1)
                        DATMOS(2) = DATMOS(1)
                        MBDELY(2) = MBDELY(1)
                        CLOCK(2)  = CLOCK(1)
                        DCLOCK(2) = DCLOCK(1)
                        DISP(2)   = DISP(1)
                        DDISP(2)  = DDISP(1)
                     END IF
C
C                    Either write output table row number OUTROW,
C                    increment OUTROW, and set IRET to 0 or issue one or
C                    more fatal error messages and set IRET to a
C                    non-zero value:
C
                     CALL OTABCL (OUTTBL, 'WRIT', OUTROW, STKDIM, IFDIM,
     *                            TIME, TIMINT, SOURID, ANTNUM, SUBARR,
     *                            FREQID, IFR, GEODLY, NEWDOP, ATMOS,
     *                            DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *                            DDISP, NEWCRE, NEWCIM, NEWDLY, NEWRAT,
     *                            NEWWT, NEWREF, IRET)
C
                     IF (IRET .NE. 0) THEN
C
C                       Failed to write row.
C
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (9)
                     END IF
                  ELSE IF (IRET .LT. 0) THEN
C
C                    Input table row number ROW was read and is flagged.
C
C                    Clear error indicator:
C
                     IRET = 0
C
                  ELSE
C
C                    Failed to read row.
C
                     WRITE (MSGTXT, 9011) IRET
                     CALL MSGWRT (9)
                  END IF
               GO TO 10
               END IF
C
C              Either close the output table and leave IRET unchanged or
C              issue one or more fatal error messages and set IRET to a
C              non-zero value:
C
               CALL OTABCL (OUTTBL, 'CLOS', OUTROW, STKDIM, IFDIM, TIME,
     *                      TIMINT, SOURID, ANTNUM, SUBARR, FREQID,
     *                      IFR, GEODLY, NEWDOP, ATMOS, DATMOS, MBDELY,
     *                      CLOCK, DCLOCK, DISP, DDISP, NEWCRE, NEWCIM,
     *                      NEWDLY, NEWRAT, NEWWT, NEWREF, IRET1)
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 9012) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
C
            ELSE
C
C              Failed to open output table.
C
               WRITE (MSGTXT, 9013) IRET
               CALL MSGWRT (9)
            END IF
C
C           Either close the input table and leave IRET unchanged or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL OTABCL (INTABL, 'CLOS', CLROW, NUMPOL, NUMIF, TIME,
     *                   TIMINT, SOURID, ANTNUM, SUBARR, FREQID, IFR,
     *                   GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK,
     *                   DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *                   WEIGHT, REFA, IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           Failed to open input table.
C
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPCL3: FAILED TO CLOSE INPUT TABLE (ERROR ', I4, ')')
 9001 FORMAT ('FXPCL3: FAILED TO OPEN INPUT TABLE (ERROR ', I4, ')')
 9010 FORMAT ('FXPCL3: FAILED TO WRITE TO OUTPUT TABLE (ERROR ', I4,
     *        ')')
 9011 FORMAT ('FXPCL3: FAILED TO READ FROM INPUT TABLE (ERROR ', I4,
     *        ')')
 9012 FORMAT ('FXPCL3: FAILED TO CLOSE OUTPUT TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXPCL3: FAILED TO OPEN OUTPUT TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPCQ (INFILE, OUTFIL, CQVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy CQ table CQVER.
C
C   If there is an CQ table with version number CQVER attached to INFILE
C   then attach a transformed copy to OUTFIL with version number CQVER
C   and set IRET to zero. If there is no CQ table with version number
C   CQVER attached to INFILE then set IRET to zero. If the table can not
C   be transformed or copied then issue a fatal error message and set
C   IRET to a non-zero value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     CQVER    I        frequency table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   CQVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input CQ table
C              (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'CQ', CQVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as CQ
C           table version number CQVER and set IRET to zero or issue one
C           or more fatal error messages and set IRET to a non-zero
C           value:
C
            CALL FXPCQ2 (INTABL, OUTFIL, CQVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPCQ: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPCQ: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPCQ2 (INTABL, OUTFIL, CQVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   CQVER.
C
C   Either attach a copy of the CQ table INTABL with IF entries
C   modified to be consistent with the current IF/polarization mapping
C   to OUTFIL as CQ table version number CQVER and set IRET to zero
C   or issue one or more fatal error messages and set IRET to a non-zero
C   value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input CQ
C                       table: must reference an existing CQ table and
C                       must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     CQVER    I        CQ table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   CQVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C     EXISTS   does the table corresponding to OUTTBL already exist?
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'CQ', CQVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If there is already a table corresponding to OUTTBL then either
C        remove it and set IRET to zero or issue one or more fatal error
C        messages and set IRET to a non-zero value. If there is no table
C        corresponding to OUTTBL then set IRET to 0.
C
         CALL TABEXI (OUTTBL, EXISTS, IRET)
         IRET = 0
         IF (EXISTS) THEN
            CALL TABRMV (OUTTBL, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Announce table being copied:
C
            WRITE (MSGTXT, 4000) CQVER
            CALL MSGWRT (4)
C
C           Either make OUTTBL a transformed copy of INTABL and set IRET
C           to zero or issue one or more fatal error messages and set
C           IRET to a non-zero value:
C
            CALL FXPCQ3 (INTABL, OUTTBL, IRET)
C
         END IF
C
C        Either delete the OUTTBL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (OUTTBL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9001) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 4000 FORMAT ('Fixing CQ table version ', I4)
 9000 FORMAT ('FXPCQ2: TABLE REMOVAL FAILURE (ERROR ', I4, ')')
 9001 FORMAT ('FXPCQ2: OBJECT DESTRUCTION FAILURE (ERROR ', I4, ')')
 9002 FORMAT ('FXPCQ2: OBJECT ALLOCATION FAILURE (ERROR ', I4, ')')
      END
      SUBROUTINE FXPCQ3 (INTABL, OUTTBL, IRET)
C-----------------------------------------------------------------------
C   Make CQ table OUTTBL a transformed copy of INTABL.
C
C   Either create OUTTBL as a copy of INTABL with the IF entries
C   re-mapped according to the current IF/polarization mapping and set
C   IRET to 0 or issue one or more fatal error messages and set IRET to
C   a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of table object used to access input CQ
C                       table: must reference an existing CQ table
C                       belonging to a file for which the
C                       IF/polarization mapping is valid
C     OUTTBL   C*(*)    name of table object used to access output CQ
C                       table: must not reference an existing table
C
C   Output:
C     IRET     I        return status: 0 if output table created,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTTBL*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     NUMROW   number of rows in INTABL
C
C     CQROW    CQ table row number
C     NUMIF    number of IFs in INTABL
C
C     ROW      number of last row read from INTABL
C     OUTROW   number of next row to write to OUTTBL
C
C     FREQID   frequency ID number
C     SUBARR   subarray number
C     FFTSIZ   FFT sizes by IF
C     NCHAN    numbers of channels by IF
C     SPECAV   spectral averaging by IF
C     EDGEFQ   edge frequencies by IF
C     CHANBW   channel bandwidths by IF
C     TAPER    taper functions by IF
C     OVRSMP   oversampling factors by IF
C     ZEROPD   zero padding by IF
C     FILTER   filter types by IF
C     AVTIME   averaging times by IF
C     NBITS    numbers of bits by IF
C     OVRLAP   FFT overlap factors by IF
C
C     IFNUM    IF number
C
C     NEWFFT   remapped FFT sizes
C     NEWNCH   remapped numbers of channels
C     NEWSPA   remapped spectral averaging
C     NEWEDG   remapped edge frequencies
C     NEWCBW   remapped channel bandwidths
C     NEWTPR   remapped taper functions
C     NEWOSM   remapped oversampling factors
C     NEWZPD   remapped zero padding
C     NEWFLT   remapped filter types
C     NEWAVT   remapped averaging times
C     NEWBIT   remapped numbers of bits
C     NEWOVR   remapped overlap factors
C
C     TYPE     attribute type code
C     DIM      attribute dimensions list
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   NUMROW
C
      INTEGER   CQROW
      INTEGER   NUMIF
C
      INTEGER   ROW
      INTEGER   OUTROW
C
      INTEGER          FREQID
      INTEGER          SUBARR
      INTEGER          FFTSIZ(MAXIF)
      INTEGER          NCHAN(MAXIF)
      INTEGER          SPECAV(MAXIF)
      DOUBLE PRECISION EDGEFQ(MAXIF)
      DOUBLE PRECISION CHANBW(MAXIF)
      CHARACTER*8      TAPER(MAXIF)
      INTEGER          OVRSMP(MAXIF)
      INTEGER          ZEROPD(MAXIF)
      INTEGER          FILTER(MAXIF)
      REAL             AVTIME(MAXIF)
      INTEGER          NBITS(MAXIF)
      INTEGER          OVRLAP(MAXIF)
C
      INTEGER   IFNUM
C
      INTEGER          NEWFFT(MAXIF)
      INTEGER          NEWNCH(MAXIF)
      INTEGER          NEWSPA(MAXIF)
      DOUBLE PRECISION NEWEDG(MAXIF)
      DOUBLE PRECISION NEWCBW(MAXIF)
      CHARACTER*8      NEWTPR(MAXIF)
      INTEGER          NEWOSM(MAXIF)
      INTEGER          NEWZPD(MAXIF)
      INTEGER          NEWFLT(MAXIF)
      REAL             NEWAVT(MAXIF)
      INTEGER          NEWBIT(MAXIF)
      INTEGER          NEWOLP(MAXIF)
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     A deficiency in the TABLE interface prevents the number of rows
C     being read if the TABLE has not been open with the generic TABOPN
C     interface so the table must opened to read the number of rows and
C     then closed before being opened by the specialized OCQINI
C     interface.
C
C     If INTABL can be opened and closed then set NUMROW to the number
C     of rows in INTABL and IRET to zero otherwise issue one or more
C     fatal error messages and set IRET to a non-zero value:
C
      CALL TABOPN (INTABL, 'READ', IRET)
      IF (IRET .EQ. 0) THEN
C
C        Set NUMROW to the number of rows in INTABL and IRET to 0:
C
         CALL TABGET (INTABL, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
         CALL TABCLO (INTABL, IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Table open failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
      IF (IRET .EQ. 0) THEN
C
C        Either make OUTTBL a copy of INTABL with IF entries remapped
C        according to the current polarization/IF mapping and set IRET
C        to 0 or issue one or more fatal error messages and set IRET to
C        a non-zero value:
C
         CALL OCQINI (INTABL, 'READ', CQROW, NUMIF, IRET)
         IF (IRET .EQ. 0) THEN
C
C           Input table was opened for reading.
C
            CALL OCQINI (OUTTBL, 'WRIT', CQROW, IFDIM, IRET)
            IF (IRET .EQ. 0) THEN
C
C              Output table was opened for writing.
C
               ROW    = 0
               OUTROW = 1
   10          IF ((IRET .EQ. 0) .AND. (ROW .NE. NUMROW)) THEN
                  ROW   = ROW + 1
                  CQROW = ROW
                  CALL OTABCQ (INTABL, 'READ', CQROW, NUMIF, FREQID,
     *                         SUBARR, FFTSIZ, NCHAN, SPECAV, EDGEFQ,
     *                         CHANBW, TAPER, OVRSMP, ZEROPD, FILTER,
     *                         AVTIME, NBITS, OVRLAP, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    Input table row number ROW was read and is not
C                    flagged.
C
                     DO 20 IFNUM = 1, NUMIF
                        NEWFFT(IFINDX(IFNUM)) = FFTSIZ(IFNUM)
                        NEWNCH(IFINDX(IFNUM)) = NCHAN(IFNUM)
                        NEWSPA(IFINDX(IFNUM)) = SPECAV(IFNUM)
                        NEWEDG(IFINDX(IFNUM)) = EDGEFQ(IFNUM)
                        NEWCBW(IFINDX(IFNUM)) = CHANBW(IFNUM)
                        NEWTPR(IFINDX(IFNUM)) = TAPER(IFNUM)
                        NEWOSM(IFINDX(IFNUM)) = OVRSMP(IFNUM)
                        NEWZPD(IFINDX(IFNUM)) = ZEROPD(IFNUM)
                        NEWFLT(IFINDX(IFNUM)) = FILTER(IFNUM)
                        NEWAVT(IFINDX(IFNUM)) = AVTIME(IFNUM)
                        NEWBIT(IFINDX(IFNUM)) = NBITS(IFNUM)
                        NEWOLP(IFINDX(IFNUM)) = OVRLAP(IFNUM)
   20                CONTINUE
C
C                    Either write output table row number OUTROW,
C                    increment OUTROW, and set IRET to 0 or issue one or
C                    more fatal error messages and set IRET to a
C                    non-zero value:
C
                     CALL OTABCQ (OUTTBL, 'WRIT', OUTROW, IFDIM,
     *                            FREQID, SUBARR, NEWFFT, NEWNCH,
     *                            NEWSPA, NEWEDG, NEWCBW, NEWTPR,
     *                            NEWOSM, NEWZPD, NEWFLT, NEWAVT,
     *                            NEWBIT, NEWOLP, IRET)
C
                     IF (IRET .NE. 0) THEN
C
C                       Failed to write row.
C
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (9)
                     END IF
                  ELSE IF (IRET .LT. 0) THEN
C
C                    Input table row number ROW was read and is flagged.
C
C                    Clear error indicator:
C
                     IRET = 0
C
                  ELSE
C
C                    Failed to read row.
C
                     WRITE (MSGTXT, 9011) IRET
                     CALL MSGWRT (9)
                  END IF
               GO TO 10
               END IF
C
C              Either close the output table and leave IRET unchanged or
C              issue one or more fatal error messages and set IRET to a
C              non-zero value:
C
               CALL OTABCQ (OUTTBL, 'CLOS', OUTROW, IFDIM, FREQID,
     *                      SUBARR, NEWFFT, NEWNCH, NEWSPA, NEWEDG,
     *                      NEWCBW, NEWTPR, NEWOSM, NEWZPD, NEWFLT,
     *                      NEWAVT, NEWBIT, NEWOLP, IRET1)
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 9012) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
C
            ELSE
C
C              Failed to open output table.
C
               WRITE (MSGTXT, 9013) IRET
               CALL MSGWRT (9)
            END IF
C
C           Either close the input table and leave IRET unchanged or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL OTABCQ (INTABL, 'CLOS', CQROW, NUMIF, FREQID, SUBARR,
     *                   FFTSIZ, NCHAN, SPECAV, EDGEFQ, CHANBW, TAPER,
     *                   OVRSMP, ZEROPD, FILTER, AVTIME, NBITS, OVRLAP,
     *                   IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           Failed to open input table.
C
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPCQ3: FAILED TO CLOSE INPUT TABLE (ERROR ', I4, ')')
 9001 FORMAT ('FXPCQ3: FAILED TO OPEN INPUT TABLE (ERROR ', I4, ')')
 9010 FORMAT ('FXPCQ3: FAILED TO WRITE TO OUTPUT TABLE (ERROR ', I4,
     *        ')')
 9011 FORMAT ('FXPCQ3: FAILED TO READ FROM INPUT TABLE (ERROR ', I4,
     *        ')')
 9012 FORMAT ('FXPCQ3: FAILED TO CLOSE OUTPUT TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXPCQ3: FAILED TO OPEN OUTPUT TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPCT (INFILE, OUTFIL, CTVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy CT table CTVER.
C
C   If there is an CT table with version number CTVER attached to INFILE
C   then attach a transformed copy to OUTFIL with version number CTVER
C   and set IRET to zero. If there is no CT table with version number
C   CTVER attached to INFILE then set IRET to zero. If the table can not
C   be transformed or copied then issue a fatal error message and set
C   IRET to a non-zero value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     CTVER    I        frequency table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   CTVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input CT table
C              (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'CT', CTVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as CT
C           table version number CTVER and set IRET to zero or issue one
C           or more fatal error messages and set IRET to a non-zero
C           value:
C
            CALL FXPCT2 (INTABL, OUTFIL, CTVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPCT: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPCT: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPCT2 (INTABL, OUTFIL, CTVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   CTVER.
C
C   Either attach a copy of the CT table INTABL with IF entries
C   modified to be consistent with the current IF/polarization mapping
C   to OUTFIL as CT table version number CTVER and set IRET to zero
C   or issue one or more fatal error messages and set IRET to a non-zero
C   value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input CT
C                       table: must reference an existing CT table and
C                       must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     CTVER    I        CT table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   CTVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C
C     NKEYS    number of header keywords to modify
C     KEYS     keywords to modify
C     KVALS    keyword value array
C     KLOCS    keyword location index
C     KTYPE    keyword type codes
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
C
      INTEGER   NKEYS
      PARAMETER (NKEYS = 3)
      CHARACTER KEYS(NKEYS)*8
      INTEGER   KVALS(2 * NKEYS)
      INTEGER   KLOCS(NKEYS)
      INTEGER   KTYPE(NKEYS)
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C
      DATA KEYS  / 'NO_STKD ', 'STK_1   ', 'NO_BAND ' /
      DATA KLOCS /  1,          2,          3         /
      DATA KTYPE /  4,          4,          4         /
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'CT', CTVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        The changes in the polarization/IF structure only affect the
C        table header so the table is simply copied to the output file
C        and then the header keywords are corrected.
C
C        Either copy INTABL to OUTTBL and set IRET to 0 or set IRET to
C        a non-zero value if the table can not be copied:
C
         CALL TBLCOP (INTABL, OUTTBL, IRET)
C
         IF (IRET .EQ. 0) THEN
C
C           Either open the output table and set IRET to zero or set
C           IRET to a non-zero value if the table can not be opened:
C
            CALL TABOPN (OUTTBL, 'WRIT', IRET)
C
            IF (IRET .EQ. 0) THEN
C
C              Either set the number of polarizations to STKDIM, set the
C              first polarization to STK1, set the number of IFs to
C              IFDIM, and set IRET to 0 or set IRET to a non-zero value
C              if the table header can not be updated:
C
               KVALS(KLOCS(1)) = STKDIM
               KVALS(KLOCS(2)) = STK1
               KVALS(KLOCS(3)) = IFDIM
               CALL TABKPT (OUTTBL, KEYS, NKEYS, KLOCS, KVALS, KTYPE,
     *                      IRET)
C
               IF (IRET .NE. 0) THEN
                  WRITE (MSGTXT, 9000) IRET
                  CALL MSGWRT (9)
               END IF
C
C              Either close the table and set IRET1 to zero or set IRET1
C              to a non-zero value if the table can not be closed:
C
               CALL TABCLO (OUTTBL, IRET1)
C
               IF (IRET1 .NE. 0) THEN
                  IRET = IRET1
                  WRITE (MSGTXT, 9001) IRET
                  CALL MSGWRT (9)
               END IF
            ELSE
C
C              Open failed.
C
               WRITE (MSGTXT, 9002) IRET
               CALL MSGWRT (9)
            END IF
         ELSE
C
C           Copy failed.
C
            WRITE (MSGTXT, 9003) IRET
            CALL MSGWRT (9)
         END IF
C
C        Either de-allocate the OUTTBL object and set IRET1 to zero or
C        set IRET1 to a non-zero value if the object can not be freed:
C
         CALL TABDES (OUTTBL, IRET1)
C
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9004) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9005) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPCT2: FAILED TO UPDATE KEYWORDS (ERROR ', I4, ')')
 9001 FORMAT ('FXPCT2: FAILED TO CLOSE TABLE (ERROR ', I4, ')')
 9002 FORMAT ('FXPCT2: FAILED TO OPEN TABLE (ERROR ', I4, ')')
 9003 FORMAT ('FXPCT2: FAILED TO COPY TABLE (ERROR ', I4, ')')
 9004 FORMAT ('FXPCT2: FAILED TO FREE TABLE OBJECT (ERROR ', I4, ')')
 9005 FORMAT ('FXPCT2: FAILED TO ALLOCATE TABLE OBJECT (ERROR ', I4,
     *        ')')
      END
      SUBROUTINE FIXPFG (INFILE, OUTFIL, FGVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy flag table FGVER.
C
C   If there is an flag table with version number FGVER attached to
C   INFILE then attach a transformed copy to OUTFIL with version number
C   FGVER and set IRET to zero. If there is no flag table with version
C   number FGVER attached to INFILE then set IRET to zero. If the table
C   can not be transformed or copied then issue a fatal error message
C   and set IRET to a non-zero value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     FGVER    I        frequency table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   FGVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input flag table
C              (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'FG', FGVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as flag
C           table version number FGVER and set IRET to zero or issue one
C           or more fatal error messages and set IRET to a non-zero
C           value:
C
            CALL FXPFG2 (INTABL, OUTFIL, FGVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPFG: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPFG: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPFG2 (INTABL, OUTFIL, FGVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   FGVER.
C
C   Either attach a copy of the flag table INTABL with IF entries
C   modified to be consistent with the current IF/polarization mapping
C   to OUTFIL as flag table version number FGVER and set IRET to zero
C   or issue one or more fatal error messages and set IRET to a non-zero
C   value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input flag
C                       table: must reference an existing flag table and
C                       must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     FGVER    I        FG table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   FGVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C     EXISTS   does the table corresponding to OUTTBL already exist?
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'FG', FGVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If there is already a table corresponding to OUTTBL then either
C        remove it and set IRET to zero or issue one or more fatal error
C        messages and set IRET to a non-zero value. If there is no table
C        corresponding to OUTTBL then set IRET to 0.
C
         CALL TABEXI (OUTTBL, EXISTS, IRET)
         IRET = 0
         IF (EXISTS) THEN
            CALL TABRMV (OUTTBL, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Announce table being copied:
C
            WRITE (MSGTXT, 4000) FGVER
            CALL MSGWRT (4)
C
C           Either make OUTTBL a transformed copy of INTABL and set IRET
C           to zero or issue one or more fatal error messages and set
C           IRET to a non-zero value:
C
            CALL FXPFG3 (INTABL, OUTTBL, IRET)
C
         END IF
C
C        Either delete the OUTTBL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (OUTTBL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9001) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 4000 FORMAT ('Fixing FG table version ', I4)
 9000 FORMAT ('FXPFG2: TABLE REMOVAL FAILURE (ERROR ', I4, ')')
 9001 FORMAT ('FXPFG2: OBJECT DESTRUCTION FAILURE (ERROR ', I4, ')')
 9002 FORMAT ('FXPFG2: OBJECT ALLOCATION FAILURE (ERROR ', I4, ')')
      END
      SUBROUTINE FXPFG3 (INTABL, OUTTBL, IRET)
C-----------------------------------------------------------------------
C   Make flag table OUTTBL a transformed copy of INTABL.
C
C   Either create OUTTBL as a copy of INTABL with the IF entries
C   re-mapped according to the current IF/polarization mapping and set
C   IRET to 0 or issue one or more fatal error messages and set IRET to
C   a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of table object used to access input flag
C                       table: must reference an existing flag table
C                       belonging to a file for which the
C                       IF/polarization mapping is valid
C     OUTTBL   C*(*)    name of table object used to access output flag
C                       table: must not reference an existing table
C
C   Output:
C     IRET     I        return status: 0 if output table created,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTTBL*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     NUMROW   number of rows in INTABL
C
C     FGROW    flag table row number
C
C     ROW      number of last row read from INTABL
C     OUTROW   number of next row to write to OUTTBL
C
C     SOURID   source ID number
C     SUBARR   subarray number
C     FREQID   frequency ID number
C     ANTENS   antenna numbers
C     TIMRNG   time range
C     IFS      IF range
C     CHANS    channel range
C     PFLAGS   polarization flags
C     REASON   reason for flag
C
C     TYPE     attribute type code
C     DIM      attribute dimensions list
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   NUMROW
C
      INTEGER   FGROW
C
      INTEGER   ROW
      INTEGER   OUTROW
C
      INTEGER   SOURID
      INTEGER   SUBARR
      INTEGER   FREQID
      INTEGER   ANTENS(2)
      REAL      TIMRNG(2)
      INTEGER   IFS(2)
      INTEGER   CHANS(2)
      LOGICAL   PFLAGS(4)
      CHARACTER REASON*24
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     A deficiency in the TABLE interface prevents the number of rows
C     being read if the TABLE has not been open with the generic TABOPN
C     interface so the table must opened to read the number of rows and
C     then closed before being opened by the specialized OFGINI
C     interface.
C
C     If INTABL can be opened and closed then set NUMROW to the number
C     of rows in INTABL and IRET to zero otherwise issue one or more
C     fatal error messages and set IRET to a non-zero value:
C
      CALL TABOPN (INTABL, 'READ', IRET)
      IF (IRET .EQ. 0) THEN
C
C        Set NUMROW to the number of rows in INTABL and IRET to 0:
C
         CALL TABGET (INTABL, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
         CALL TABCLO (INTABL, IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Table open failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
      IF (IRET .EQ. 0) THEN
C
C        Either make OUTTBL a copy of INTABL with IF entries remapped
C        according to the current polarization/IF mapping and set IRET
C        to 0 or issue one or more fatal error messages and set IRET to
C        a non-zero value:
C
         CALL OFGINI (INTABL, 'READ', FGROW, IRET)
         IF (IRET .EQ. 0) THEN
C
C           Input table was opened for reading.
C
            CALL OFGINI (OUTTBL, 'WRIT', FGROW, IRET)
            IF (IRET .EQ. 0) THEN
C
C              Output table was opened for writing.
C
               ROW    = 0
               OUTROW = 1
   10          IF ((IRET .EQ. 0) .AND. (ROW .NE. NUMROW)) THEN
                  ROW   = ROW + 1
                  FGROW = ROW
                  CALL OTABFG (INTABL, 'READ', FGROW, SOURID, SUBARR,
     *                         FREQID, ANTENS, TIMRNG, IFS, CHANS,
     *                         PFLAGS, REASON, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    Input table row number ROW was read and is not
C                    flagged.
C
                     IF (IFS(1) .GT. 0) THEN
                        IFS(1) = IFINDX(IFS(1))
                     END IF
                     IF (IFS(2) .GT. 0) THEN
                        IFS(2) = IFINDX(IFS(2))
                     END IF
                     IF (STKDIM .GT. 1) THEN
                        PFLAGS(2) = PFLAGS(1)
                     END IF
C
C                    Either write output table row number OUTROW,
C                    increment OUTROW, and set IRET to 0 or issue one or
C                    more fatal error messages and set IRET to a
C                    non-zero value:
C
                     CALL OTABFG (OUTTBL, 'WRIT', OUTROW, SOURID,
     *                            SUBARR, FREQID, ANTENS, TIMRNG,
     *                            IFS, CHANS, PFLAGS, REASON, IRET)
C
                     IF (IRET .NE. 0) THEN
C
C                       Failed to write row.
C
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (9)
                     END IF
                  ELSE IF (IRET .LT. 0) THEN
C
C                    Input table row number ROW was read and is flagged.
C
C                    Clear error indicator:
C
                     IRET = 0
C
                  ELSE
C
C                    Failed to read row.
C
                     WRITE (MSGTXT, 9011) IRET
                     CALL MSGWRT (9)
                  END IF
               GO TO 10
               END IF
C
C              Either close the output table and leave IRET unchanged or
C              issue one or more fatal error messages and set IRET to a
C              non-zero value:
C
               CALL OTABFG (OUTTBL, 'CLOS', OUTROW, SOURID, SUBARR,
     *                      FREQID, ANTENS, TIMRNG, IFS, CHANS, PFLAGS,
     *                      REASON, IRET1)
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 9012) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
C
            ELSE
C
C              Failed to open output table.
C
               WRITE (MSGTXT, 9013) IRET
               CALL MSGWRT (9)
            END IF
C
C           Either close the input table and leave IRET unchanged or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL OTABFG (INTABL, 'CLOS', FGROW, SOURID, SUBARR, FREQID,
     *                   ANTENS, TIMRNG, IFS, CHANS, PFLAGS, REASON,
     *                   IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           Failed to open input table.
C
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPFG3: FAILED TO CLOSE INPUT TABLE (ERROR ', I4, ')')
 9001 FORMAT ('FXPFG3: FAILED TO OPEN INPUT TABLE (ERROR ', I4, ')')
 9010 FORMAT ('FXPFG3: FAILED TO WRITE TO OUTPUT TABLE (ERROR ', I4,
     *        ')')
 9011 FORMAT ('FXPFG3: FAILED TO READ FROM INPUT TABLE (ERROR ', I4,
     *        ')')
 9012 FORMAT ('FXPFG3: FAILED TO CLOSE OUTPUT TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXPFG3: FAILED TO OPEN OUTPUT TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPGC (INFILE, OUTFIL, GCVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy gain curve table GCVER.
C
C   If there is a gain curve table with version number GCVER attached to
C   INFILE then attach a transformed copy to OUTFIL with version number
C   GCVER and set IRET to zero. If there is no gain curve table with
C   version number GCVER attached to INFILE then set IRET to zero. If
C   the table can not be transformed or copied then issue a fatal error
C   message and set IRET to a non-zero value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     GCVER    I        frequency table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   GCVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input gain curve
C              table (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'GC', GCVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as gain
C           curve table version number GCVER and set IRET to zero or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL FXPGC2 (INTABL, OUTFIL, GCVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPGC: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPGC: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPGC2 (INTABL, OUTFIL, GCVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   GCVER.
C
C   Either attach a copy of the gain curve table INTABL with IF entries
C   modified to be consistent with the current IF/polarization mapping
C   to OUTFIL as gain curve table version number GCVER and set IRET to
C   zero or issue one or more fatal error messages and set IRET to a
C   non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input GC
C                       table: must reference an existing gain curve
C                       table and must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     GCVER    I        GC table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   GCVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C     EXISTS   does the table corresponding to OUTTBL already exist?
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'GC', GCVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If there is already a table corresponding to OUTTBL then either
C        remove it and set IRET to zero or issue one or more fatal error
C        messages and set IRET to a non-zero value. If there is no table
C        corresponding to OUTTBL then set IRET to 0.
C
         CALL TABEXI (OUTTBL, EXISTS, IRET)
         IRET = 0
         IF (EXISTS) THEN
            CALL TABRMV (OUTTBL, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Announce table being copied:
C
            WRITE (MSGTXT, 4000) GCVER
            CALL MSGWRT (4)
C
C           Either make OUTTBL a transformed copy of INTABL and set IRET
C           to zero or issue one or more fatal error messages and set
C           IRET to a non-zero value:
C
            CALL FXPGC3 (INTABL, OUTTBL, IRET)
C
         END IF
C
C        Either delete the OUTTBL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (OUTTBL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9001) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 4000 FORMAT ('Fixing GC table version ', I4)
 9000 FORMAT ('FXPGC2: TABLE REMOVAL FAILURE (ERROR ', I4, ')')
 9001 FORMAT ('FXPGC2: OBJECT DESTRUCTION FAILURE (ERROR ', I4, ')')
 9002 FORMAT ('FXPGC2: OBJECT ALLOCATION FAILURE (ERROR ', I4, ')')
      END
      SUBROUTINE FXPGC3 (INTABL, OUTTBL, IRET)
C-----------------------------------------------------------------------
C   Make gain curve table OUTTBL a transformed copy of INTABL.
C
C   Either create OUTTBL as a copy of INTABL with the IF entries
C   re-mapped according to the current IF/polarization mapping and set
C   IRET to 0 or issue one or more fatal error messages and set IRET to
C   a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of table object used to access input GC
C                       table: must reference an existing GC table
C                       belonging to a file for which the
C                       IF/polarization mapping is valid
C     OUTTBL   C*(*)    name of table object used to access output GC
C                       table: must not reference an existing table
C
C   Output:
C     IRET     I        return status: 0 if output table created,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTTBL*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     NUMROW   number of rows in INTABL
C
C     GCROW    GC table row number
C     NUMPOL   number of polarizations in INTABL
C     NUMIF    number of IFs in INTABL
C     NUMTAB   number of tabulated values in INTABL
C
C     ROW      number of last row read from INTABL
C     OUTROW   number of next row to write to OUTTBL
C
C     MAXTAB   maximum number of tabulated values
C     ANTNUM   solution antenna number
C     SUBARR   solution subarray number
C     FREQID   solution frequency ID
C     GCTYPE   gain curve type by polarization and IF
C     NTERMS   number of tabulated terms by polarization and IF
C     XTYPE    x value type by polarization and IF
C     YTYPE    y value type by polarization and IF
C     XVALUE   x values by polarization and IF
C     YVALUE   tabulated y values by polarization and IF
C     RGAIN    tabulated relative gains by polarization and IF
C     SENS     sensitivities by polarization and IF
C
C     IFNUM    IF number
C     TERM     term number
C
C     NEWTYP   remapped gain curve types
C     NEWNTM   remapped numbers of terms
C     NEWXTY   remapped x types
C     NEWYTY   remapped y types
C     NEWXVA   remapped x values
C     NEWYVA   remapped y values
C     NEWGAI   remapped gains
C     NEWSEN   remapped sensitivities
C
C     TYPE     attribute type code
C     DIM      attribute dimensions list
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   NUMROW
C
      INTEGER   GCROW
      INTEGER   NUMPOL
      INTEGER   NUMIF
      INTEGER   NUMTAB
C
      INTEGER   ROW
      INTEGER   OUTROW
C
      INTEGER   MAXTAB
      PARAMETER (MAXTAB = 200)
      INTEGER   ANTNUM
      INTEGER   SUBARR
      INTEGER   FREQID
      INTEGER   GCTYPE(2, MAXIF)
      INTEGER   NTERMS(2, MAXIF)
      INTEGER   XTYPE(2, MAXIF)
      INTEGER   YTYPE(2, MAXIF)
      REAL      XVALUE(2, MAXIF)
      REAL      YVALUE(2, MAXIF, MAXTAB)
      REAL      RGAIN(2, MAXIF, MAXTAB)
      REAL      SENS(2, MAXIF)
C
      INTEGER   IFNUM
      INTEGER   TERM
C
      INTEGER   NEWTYP(2, MAXIF)
      INTEGER   NEWNTM(2, MAXIF)
      INTEGER   NEWXTY(2, MAXIF)
      INTEGER   NEWYTY(2, MAXIF)
      REAL      NEWXVA(2, MAXIF)
      REAL      NEWYVA(2, MAXIF, MAXTAB)
      REAL      NEWGAI(2, MAXIF, MAXTAB)
      REAL      NEWSEN(2, MAXIF)
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     Fill the remapped arrays with null values. Those values in the
C     range of the polarization/IF mapping function will be over-ridden
C     by values from the source CL table later in this routine.
C
      CALL FILL (2 * MAXIF, -1, NEWTYP)
      CALL FILL (2 * MAXIF, -1, NEWNTM)
      CALL FILL (2 * MAXIF, -1, NEWXTY)
      CALL FILL (2 * MAXIF, -1, NEWYTY)
      CALL RFILL (2 * MAXIF, FBLANK, NEWXVA)
      CALL RFILL (2 * MAXIF * MAXTAB, FBLANK, NEWYVA)
      CALL RFILL (2 * MAXIF * MAXTAB, FBLANK, NEWGAI)
      CALL RFILL (2 * MAXIF, FBLANK, NEWSEN)
C-----------------------------------------------------------------------
C
C     A deficiency in the TABLE interface prevents the number of rows
C     being read if the TABLE has not been open with the generic TABOPN
C     interface so the table must opened to read the number of rows and
C     then closed before being opened by the specialized OGCINI
C     interface.
C
C     If INTABL can be opened and closed then set NUMROW to the number
C     of rows in INTABL and IRET to zero otherwise issue one or more
C     fatal error messages and set IRET to a non-zero value:
C
      CALL TABOPN (INTABL, 'READ', IRET)
      IF (IRET .EQ. 0) THEN
C
C        Set NUMROW to the number of rows in INTABL and IRET to 0:
C
         CALL TABGET (INTABL, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
         CALL TABCLO (INTABL, IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Table open failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
      IF (IRET .EQ. 0) THEN
C
C        Either make OUTTBL a copy of INTABL with IF entries remapped
C        according to the current polarization/IF mapping and set IRET
C        to 0 or issue one or more fatal error messages and set IRET to
C        a non-zero value:
C
         CALL OGCINI (INTABL, 'READ', GCROW, NUMPOL, NUMIF, NUMTAB,
     *                IRET)
         IF (IRET .EQ. 0) THEN
C
C           Input table was opened for reading.
C
            CALL OGCINI (OUTTBL, 'WRIT', GCROW, STKDIM, IFDIM, NUMTAB,
     *                   IRET)
            IF (IRET .EQ. 0) THEN
C
C              Output table was opened for writing.
C
               ROW    = 0
               OUTROW = 1
   10          IF ((IRET .EQ. 0) .AND. (ROW .NE. NUMROW)) THEN
                  ROW   = ROW + 1
                  GCROW = ROW
                  CALL OTABGC (INTABL, 'READ', GCROW, NUMPOL, NUMTAB,
     *                         ANTNUM, SUBARR, FREQID, GCTYPE, NTERMS,
     *                         XTYPE, YTYPE, XVALUE, YVALUE, RGAIN,
     *                         SENS, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    Input table row number ROW was read and is not
C                    flagged.
C
                     DO 30 IFNUM = 1, NUMIF
                        NEWTYP(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = GCTYPE(1, IFNUM)
                        NEWNTM(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = NTERMS(1, IFNUM)
                        NEWXTY(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = XTYPE(1, IFNUM)
                        NEWYTY(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = YTYPE(1, IFNUM)
                        NEWXVA(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = XVALUE(1, IFNUM)
                        NEWSEN(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = SENS(1, IFNUM)
                        DO 20 TERM = 1, NUMTAB
                           NEWYVA(STKIDX(IFNUM), IFINDX(IFNUM), TERM) =
     *                        YVALUE(1, IFNUM, TERM)
                           NEWGAI(STKIDX(IFNUM), IFINDX(IFNUM), TERM) =
     *                        RGAIN(1, IFNUM, TERM)
   20                   CONTINUE
   30                CONTINUE
C
C                    Either write output table row number OUTROW,
C                    increment OUTROW, and set IRET to 0 or issue one or
C                    more fatal error messages and set IRET to a
C                    non-zero value:
C
                     CALL OTABGC (OUTTBL, 'WRIT', OUTROW, STKDIM,
     *                            NUMTAB, ANTNUM, SUBARR, FREQID,
     *                            NEWTYP, NEWNTM, NEWXTY, NEWYTY,
     *                            NEWXVA, NEWYVA, NEWGAI, NEWSEN, IRET)
C
                     IF (IRET .NE. 0) THEN
C
C                       Failed to write row.
C
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (9)
                     END IF
                  ELSE IF (IRET .LT. 0) THEN
C
C                    Input table row number ROW was read and is flagged.
C
C                    Clear error indicator:
C
                     IRET = 0
C
                  ELSE
C
C                    Failed to read row.
C
                     WRITE (MSGTXT, 9011) IRET
                     CALL MSGWRT (9)
                  END IF
               GO TO 10
               END IF
C
C              Either close the output table and leave IRET unchanged or
C              issue one or more fatal error messages and set IRET to a
C              non-zero value:
C
               CALL OTABGC (OUTTBL, 'CLOS', OUTROW, STKDIM, NUMTAB,
     *                      ANTNUM, SUBARR, FREQID, NEWTYP, NEWNTM,
     *                      NEWXTY, NEWYTY, NEWXVA, NEWYVA, NEWGAI,
     *                      NEWSEN, IRET1)
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 9012) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
C
            ELSE
C
C              Failed to open output table.
C
               WRITE (MSGTXT, 9013) IRET
               CALL MSGWRT (9)
            END IF
C
C           Either close the input table and leave IRET unchanged or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL OTABGC (INTABL, 'CLOS', GCROW, NUMPOL, NUMTAB, ANTNUM,
     *                   SUBARR, FREQID, GCTYPE, NTERMS, XTYPE,
     *                   YTYPE, XVALUE, YVALUE, RGAIN, SENS, IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           Failed to open input table.
C
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPGC3: FAILED TO CLOSE INPUT TABLE (ERROR ', I4, ')')
 9001 FORMAT ('FXPGC3: FAILED TO OPEN INPUT TABLE (ERROR ', I4, ')')
 9010 FORMAT ('FXPGC3: FAILED TO WRITE TO OUTPUT TABLE (ERROR ', I4,
     *        ')')
 9011 FORMAT ('FXPGC3: FAILED TO READ FROM INPUT TABLE (ERROR ', I4,
     *        ')')
 9012 FORMAT ('FXPGC3: FAILED TO CLOSE OUTPUT TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXPGC3: FAILED TO OPEN OUTPUT TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPIM (INFILE, OUTFIL, IMVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy interferometer model table IMVER.
C
C   If there is an interferometer model table with version number IMVER
C   attached to INFILE then attach a transformed copy to OUTFIL with
C   version number IMVER and set IRET to zero. If there is no
C   interferometer model table with version number IMVER attached to
C   INFILE then set IRET to zero. If the table can not be transformed
C   or copied then issue a fatal error message and set IRET to a non-zero
C   value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     IMVER    I        frequency table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   IMVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input interferometer
C              model table (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'IM', IMVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as
C           interferometer model table version number IMVER and set
C           IRET to zero or issue one or more fatal error messages and
C           set IRET to a non-zero value:
C
            CALL FXPIM2 (INTABL, OUTFIL, IMVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPIM: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPIM: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPIM2 (INTABL, OUTFIL, IMVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   IMVER.
C
C   Either attach a copy of the interferometer model table INTABL with
C   IF entries modified to be consistent with the current
C   IF/polarization mapping to OUTFIL as interferometer model table
C   version number IMVER and set IRET to zero or issue one or more fatal
C   error messages and set IRET to a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input IM
C                       table: must reference an existing interferometer
C                       model table and must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     IMVER    I        IM table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   IMVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C     EXISTS   does the table corresponding to OUTTBL already exist?
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'IM', IMVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If there is already a table corresponding to OUTTBL then either
C        remove it and set IRET to zero or issue one or more fatal error
C        messages and set IRET to a non-zero value. If there is no table
C        corresponding to OUTTBL then set IRET to 0.
C
         CALL TABEXI (OUTTBL, EXISTS, IRET)
         IRET = 0
         IF (EXISTS) THEN
            CALL TABRMV (OUTTBL, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Announce table being copied:
C
            WRITE (MSGTXT, 4000) IMVER
            CALL MSGWRT (4)
C
C           Either make OUTTBL a transformed copy of INTABL and set IRET
C           to zero or issue one or more fatal error messages and set
C           IRET to a non-zero value:
C
            CALL FXPIM3 (INTABL, OUTTBL, IRET)
C
         END IF
C
C        Either delete the OUTTBL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (OUTTBL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9001) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 4000 FORMAT ('Fixing IM table version ', I4)
 9000 FORMAT ('FXPIM2: TABLE REMOVAL FAILURE (ERROR ', I4, ')')
 9001 FORMAT ('FXPIM2: OBJECT DESTRUCTION FAILURE (ERROR ', I4, ')')
 9002 FORMAT ('FXPIM2: OBJECT ALLOCATION FAILURE (ERROR ', I4, ')')
      END
      SUBROUTINE FXPIM3 (INTABL, OUTTBL, IRET)
C-----------------------------------------------------------------------
C   Make gain curve table OUTTBL a transformed copy of INTABL.
C
C   Either create OUTTBL as a copy of INTABL with the IF entries
C   re-mapped according to the current IF/polarization mapping and set
C   IRET to 0 or issue one or more fatal error messages and set IRET to
C   a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of table object used to access input IM
C                       table: must reference an existing IM table
C                       belonging to a file for which the
C                       IF/polarization mapping is valid
C     OUTTBL   C*(*)    name of table object used to access output IM
C                       table: must not reference an existing table
C
C   Output:
C     IRET     I        return status: 0 if output table created,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTTBL*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     NUMROW   number of rows in INTABL
C
C     IMROW    IM table row number
C     OBSCOD   observing code
C     RDATE    reference date string
C     NUMSTK   length of STOKES axis
C     POL1     STOKES axis reference value
C     NUMIF    length of IF axis
C     NUMCHN   length of FREQ axis
C     REFFRQ   FREQ axis reference value
C     CHANBW   FREQ axis increment
C     REFPIX   FREQ axis reference pixel
C     NUMPOL   number of polarizations in INTABL
C     NUMPLY   number of ploynomial terms
C     CORREV   correlator revision number
C
C     ROW      number of last row read from INTABL
C     OUTROW   number of next row to write to OUTTBL
C
C     MAXPLY   maximum number of polynomial terms
C     TIME     time
C     TIMINT   time interval
C     SOURID   source ID
C     ANTNUM   solution antenna number
C     SUBARR   solution subarray number
C     FREQID   solution frequency ID
C     IFR      ionospheric Faraday rotation
C     FREQVR   time-dependent frequency offsets by IF
C     PDELAY   phase delay polynomials by polarization and IF
C     GDELAY   group delay polynomials by polarization
C     PRATE    phase rate polynomials by polarization and IF
C     GRATE    group rate polynomials by polarization
C     DISP     dispersive delay
C     DDISP    rate of change of dispersive delay
C
C     IFNUM    IF number
C     TERM     term number
C
C     NEWFVR   remapped phase offsets
C     NEWPDL   remapped phase delays
C     NEWPRT   remapped phase rates
C
C     TYPE     attribute type code
C     DIM      attribute dimensions list
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   NUMROW
C
      INTEGER          IMROW
      CHARACTER        OBSCOD*8
      CHARACTER        RDATE*8
      INTEGER          NUMSTK
      INTEGER          POL1
      INTEGER          NUMIF
      INTEGER          NUMCHN
      DOUBLE PRECISION REFFRQ
      DOUBLE PRECISION CHANBW
      DOUBLE PRECISION REFPIX
      INTEGER          NUMPOL
      INTEGER          NUMPLY
      DOUBLE PRECISION CORREV
C
      INTEGER   ROW
      INTEGER   OUTROW
C
      INTEGER          MAXPLY
      PARAMETER        (MAXPLY = 20)
      DOUBLE PRECISION TIME
      REAL             TIMINT
      INTEGER          SOURID
      INTEGER          ANTNUM
      INTEGER          SUBARR
      INTEGER          FREQID
      REAL             IFR
      REAL             FREQVR(MAXIF)
      DOUBLE PRECISION PDELAY(2, MAXIF, MAXPLY)
      DOUBLE PRECISION GDELAY(2, MAXPLY)
      DOUBLE PRECISION PRATE(2, MAXIF, MAXPLY)
      DOUBLE PRECISION GRATE(2, MAXPLY)
      REAL             DISP
      REAL             DDISP
C
      INTEGER   IFNUM
      INTEGER   TERM
C
      REAL             NEWFVR(MAXIF)
      DOUBLE PRECISION NEWPDL(2, MAXIF, MAXPLY)
      DOUBLE PRECISION NEWPRT(2, MAXIF, MAXPLY)
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     Fill the remapped arrays with null values. Those values in the
C     range of the polarization/IF mapping function will be over-ridden
C     by values from the source IM table later in this routine.
C
      CALL DFILL (2 * MAXIF * MAXPLY, DBLANK, NEWPDL)
      CALL DFILL (2 * MAXIF * MAXPLY, DBLANK, NEWPRT)
C-----------------------------------------------------------------------
C
C     A deficiency in the TABLE interface prevents the number of rows
C     being read if the TABLE has not been open with the generic TABOPN
C     interface so the table must opened to read the number of rows and
C     then closed before being opened by the specialized OIMINI
C     interface.
C
C     If INTABL can be opened and closed then set NUMROW to the number
C     of rows in INTABL and IRET to zero otherwise issue one or more
C     fatal error messages and set IRET to a non-zero value:
C
      CALL TABOPN (INTABL, 'READ', IRET)
      IF (IRET .EQ. 0) THEN
C
C        Set NUMROW to the number of rows in INTABL and IRET to 0:
C
         CALL TABGET (INTABL, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
         CALL TABCLO (INTABL, IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Table open failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
      IF (IRET .EQ. 0) THEN
C
C        Either make OUTTBL a copy of INTABL with IF entries remapped
C        according to the current polarization/IF mapping and set IRET
C        to 0 or issue one or more fatal error messages and set IRET to
C        a non-zero value:
C
         CALL OIMINT (INTABL, 'READ', IMROW, OBSCOD, RDATE, NUMSTK,
     *                POL1, NUMIF, NUMCHN, REFFRQ, CHANBW, REFPIX,
     *                NUMPOL, NUMPLY, CORREV, IRET)
         IF (IRET .EQ. 0) THEN
C
C           Input table was opened for reading.
C
            CALL OIMINT (OUTTBL, 'WRIT', IMROW, OBSCOD, RDATE, STKDIM,
     *                   STK1, IFDIM, NUMCHN, REFFRQ, CHANBW, REFPIX,
     *                   STKDIM, NUMPLY, CORREV, IRET)
            IF (IRET .EQ. 0) THEN
C
C              Output table was opened for writing.
C
               ROW    = 0
               OUTROW = 1
   10          IF ((IRET .EQ. 0) .AND. (ROW .NE. NUMROW)) THEN
                  ROW   = ROW + 1
                  IMROW = ROW
                  CALL OTABIM (INTABL, 'READ', IMROW, NUMPOL, TIME,
     *                         TIMINT, SOURID, ANTNUM, SUBARR, FREQID,
     *                         IFR, FREQVR, PDELAY, GDELAY, PRATE,
     *                         GRATE, DISP, DDISP, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    Input table row number ROW was read and is not
C                    flagged.
C
                     DO 30 IFNUM = 1, NUMIF
                        NEWFVR(IFINDX(IFNUM)) = FREQVR(IFNUM)
                        DO 20 TERM = 1, NUMPLY
                           NEWPDL(STKIDX(IFNUM), IFINDX(IFNUM), TERM) =
     *                        PDELAY(1, IFNUM, TERM)
                           NEWPRT(STKIDX(IFNUM), IFINDX(IFNUM), TERM) =
     *                        PRATE(1, IFNUM, TERM)
   20                   CONTINUE
   30                CONTINUE
                     IF (STKDIM .EQ. 2) THEN
                        DO 40 TERM = 1, NUMPLY
                                GDELAY(2, TERM) = GDELAY(1, TERM)
                                GRATE(2, TERM)  = GRATE(1, TERM)
   40                   CONTINUE
                     END IF
C
C                    Either write output table row number OUTROW,
C                    increment OUTROW, and set IRET to 0 or issue one or
C                    more fatal error messages and set IRET to a
C                    non-zero value:
C
                     CALL OTABIM (OUTTBL, 'WRIT', OUTROW, STKDIM,
     *                            TIME, TIMINT, SOURID, ANTNUM, SUBARR,
     *                            FREQID, IFR, NEWFVR, NEWPDL, GDELAY,
     *                            NEWPRT, GRATE, DISP, DDISP, IRET)
C
                     IF (IRET .NE. 0) THEN
C
C                       Failed to write row.
C
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (9)
                     END IF
                  ELSE IF (IRET .LT. 0) THEN
C
C                    Input table row number ROW was read and is flagged.
C
C                    Clear error indicator:
C
                     IRET = 0
C
                  ELSE
C
C                    Failed to read row.
C
                     WRITE (MSGTXT, 9011) IRET
                     CALL MSGWRT (9)
                  END IF
               GO TO 10
               END IF
C
C              Either close the output table and leave IRET unchanged or
C              issue one or more fatal error messages and set IRET to a
C              non-zero value:
C
               CALL OTABIM (OUTTBL, 'CLOS', OUTROW, STKDIM, TIME,
     *                      TIMINT, SOURID, ANTNUM, SUBARR, FREQID, IFR,
     *                      NEWFVR, NEWPDL, GDELAY, NEWPRT, GRATE,
     *                      DISP, DDISP, IRET1)
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 9012) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
C
            ELSE
C
C              Failed to open output table.
C
               WRITE (MSGTXT, 9013) IRET
               CALL MSGWRT (9)
            END IF
C
C           Either close the input table and leave IRET unchanged or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL OTABIM (INTABL, 'CLOS', IMROW, NUMPOL, TIME, TIMINT,
     *                   SOURID, ANTNUM, SUBARR, FREQID, IFR, FREQVR,
     *                   PDELAY, GDELAY, PRATE, GRATE, DISP, DDISP,
     *                   IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           Failed to open input table.
C
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPIM3: FAILED TO CLOSE INPUT TABLE (ERROR ', I4, ')')
 9001 FORMAT ('FXPIM3: FAILED TO OPEN INPUT TABLE (ERROR ', I4, ')')
 9010 FORMAT ('FXPIM3: FAILED TO WRITE TO OUTPUT TABLE (ERROR ', I4,
     *        ')')
 9011 FORMAT ('FXPIM3: FAILED TO READ FROM INPUT TABLE (ERROR ', I4,
     *        ')')
 9012 FORMAT ('FXPIM3: FAILED TO CLOSE OUTPUT TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXPIM3: FAILED TO OPEN OUTPUT TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPMC (INFILE, OUTFIL, MCVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy model components table MCVER.
C
C   If there is a model components table with version number MCVER
C   attached to INFILE then attach a transformed copy to OUTFIL with
C   version number MCVER and set IRET to zero. If there is no
C   model components table with version number MCVER attached to
C   INFILE then set IRET to zero. If the table can not be transformed
C   or copied then issue a fatal error message and set IRET to a non-zero
C   value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     MCVER    I        frequency table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   MCVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input model
C              components table (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'MC', MCVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as
C           model components table version number MCVER and set
C           IRET to zero or issue one or more fatal error messages and
C           set IRET to a non-zero value:
C
            CALL FXPMC2 (INTABL, OUTFIL, MCVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPMC: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPMC: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPMC2 (INTABL, OUTFIL, MCVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   MCVER.
C
C   Either attach a copy of the model components table INTABL with
C   IF entries modified to be consistent with the current
C   IF/polarization mapping to OUTFIL as model components table
C   version number MCVER and set IRET to zero or issue one or more fatal
C   error messages and set IRET to a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input MC
C                       table: must reference an existing model
C                       components table and must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     MCVER    I        IM table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   MCVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C     EXISTS   does the table corresponding to OUTTBL already exist?
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'MC', MCVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If there is already a table corresponding to OUTTBL then either
C        remove it and set IRET to zero or issue one or more fatal error
C        messages and set IRET to a non-zero value. If there is no table
C        corresponding to OUTTBL then set IRET to 0.
C
         CALL TABEXI (OUTTBL, EXISTS, IRET)
         IRET = 0
         IF (EXISTS) THEN
            CALL TABRMV (OUTTBL, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Announce table being copied:
C
            WRITE (MSGTXT, 4000) MCVER
            CALL MSGWRT (4)
C
C           Either make OUTTBL a transformed copy of INTABL and set IRET
C           to zero or issue one or more fatal error messages and set
C           IRET to a non-zero value:
C
            CALL FXPMC3 (INTABL, OUTTBL, IRET)
C
         END IF
C
C        Either delete the OUTTBL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (OUTTBL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9001) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 4000 FORMAT ('Fixing MC table version ', I4)
 9000 FORMAT ('FXPMC2: TABLE REMOVAL FAILURE (ERROR ', I4, ')')
 9001 FORMAT ('FXPMC2: OBJECT DESTRUCTION FAILURE (ERROR ', I4, ')')
 9002 FORMAT ('FXPMC2: OBJECT ALLOCATION FAILURE (ERROR ', I4, ')')
      END
      SUBROUTINE FXPMC3 (INTABL, OUTTBL, IRET)
C-----------------------------------------------------------------------
C   Make model components table OUTTBL a transformed copy of INTABL.
C
C   Either create OUTTBL as a copy of INTABL with the IF entries
C   re-mapped according to the current IF/polarization mapping and set
C   IRET to 0 or issue one or more fatal error messages and set IRET to
C   a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of table object used to access input MC
C                       table: must reference an existing MC table
C                       belonging to a file for which the
C                       IF/polarization mapping is valid
C     OUTTBL   C*(*)    name of table object used to access output MC
C                       table: must not reference an existing table
C
C   Output:
C     IRET     I        return status: 0 if output table created,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTTBL*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     NUMROW   number of rows in INTABL
C
C     MCROW    MC table row number
C     OBSCOD   observing code
C     RDATE    reference date string
C     NUMSTK   length of STOKES axis
C     POL1     STOKES axis reference value
C     NUMIF    length of IF axis
C     NUMCHN   length of FREQ axis
C     REFFRQ   FREQ axis reference value
C     CHANBW   FREQ axis increment
C     REFPIX   FREQ axis reference pixel
C     NUMPOL   number of polarizations in INTABL
C     FFTSIZ   FFT size
C     OVRSMP   oversampling factor
C     ZEROPD   zero padding
C     TAPER    taper function
C
C     ROW      number of last row read from INTABL
C     OUTROW   number of next row to write to OUTTBL
C
C     TIME     time
C     SOURID   source ID
C     ANTNUM   solution antenna number
C     SUBARR   solution subarray number
C     FREQID   solution frequency ID
C     ATMOS    atmospheric delay
C     DATMOS   atmospheric delay derivative
C     GDELAY   group delay
C     GRATE    group rate
C     CLOCK    clock offsets by polarizations
C     DCLOCK   clock drift rates by polarization
C     LOOFF    LO offsets by polarization and IF
C     DLOOFF   LO offset drifts by polarization and IF
C     DISP     dispersive delays by IF
C     DDISP    rates of change of dispersive delay by IF
C
C     IFNUM    IF number
C
C     NEWOFF   remapped frequency offsets
C     NEWDOF   remapped frequency offset derivatives
C
C     TYPE     attribute type code
C     DIM      attribute dimensions list
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   NUMROW
C
      INTEGER          MCROW
      CHARACTER        OBSCOD*8
      CHARACTER        RDATE*8
      INTEGER          NUMSTK
      INTEGER          POL1
      INTEGER          NUMIF
      INTEGER          NUMCHN
      DOUBLE PRECISION REFFRQ
      DOUBLE PRECISION CHANBW
      DOUBLE PRECISION REFPIX
      INTEGER          NUMPOL
      INTEGER          FFTSIZ
      INTEGER          OVRSMP
      INTEGER          ZEROPD
      CHARACTER        TAPER*8
C
      INTEGER   ROW
      INTEGER   OUTROW
C
      DOUBLE PRECISION TIME
      INTEGER          SOURID
      INTEGER          ANTNUM
      INTEGER          SUBARR
      INTEGER          FREQID
      DOUBLE PRECISION ATMOS
      DOUBLE PRECISION DATMOS
      DOUBLE PRECISION GDELAY
      DOUBLE PRECISION GRATE
      DOUBLE PRECISION CLOCK(2)
      DOUBLE PRECISION DCLOCK(2)
      REAL             DELTAT
      REAL             LOOFF(2, MAXIF)
      REAL             DLOOFF(2, MAXIF)
      REAL             DISP(2)
      REAL             DDISP(2)
C
      INTEGER   IFNUM
C
      REAL      NEWOFF(2, MAXIF)
      REAL      NEWDOF(2, MAXIF)
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     Fill the remapped arrays with null values. Those values in the
C     range of the polarization/IF mapping function will be over-ridden
C     by values from the source IM table later in this routine.
C
      CALL RFILL (2 * MAXIF, FBLANK, NEWOFF)
      CALL RFILL (2 * MAXIF, FBLANK, NEWDOF)
C-----------------------------------------------------------------------
C
C     A deficiency in the TABLE interface prevents the number of rows
C     being read if the TABLE has not been open with the generic TABOPN
C     interface so the table must opened to read the number of rows and
C     then closed before being opened by the specialized OMCINI
C     interface.
C
C     If INTABL can be opened and closed then set NUMROW to the number
C     of rows in INTABL and IRET to zero otherwise issue one or more
C     fatal error messages and set IRET to a non-zero value:
C
      CALL TABOPN (INTABL, 'READ', IRET)
      IF (IRET .EQ. 0) THEN
C
C        Set NUMROW to the number of rows in INTABL and IRET to 0:
C
         CALL TABGET (INTABL, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
         CALL TABCLO (INTABL, IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Table open failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
      IF (IRET .EQ. 0) THEN
C
C        Either make OUTTBL a copy of INTABL with IF entries remapped
C        according to the current polarization/IF mapping and set IRET
C        to 0 or issue one or more fatal error messages and set IRET to
C        a non-zero value:
C
         CALL OMCINI (INTABL, 'READ', MCROW, OBSCOD, RDATE, NUMSTK,
     *      POL1, NUMIF, NUMCHN, REFFRQ, CHANBW, REFPIX, NUMPOL, FFTSIZ,
     *      OVRSMP, ZEROPD, TAPER, DELTAT, IRET)
         IF (IRET .EQ. 0) THEN
C
C           Input table was opened for reading.
C
            CALL OMCINI (OUTTBL, 'WRIT', MCROW, OBSCOD, RDATE, STKDIM,
     *         STK1, IFDIM, NUMCHN, REFFRQ, CHANBW, REFPIX, STKDIM,
     *         FFTSIZ, OVRSMP, ZEROPD, TAPER, DELTAT, IRET)
            IF (IRET .EQ. 0) THEN
C
C              Output table was opened for writing.
C
               ROW    = 0
               OUTROW = 1
   10          IF ((IRET .EQ. 0) .AND. (ROW .NE. NUMROW)) THEN
                  ROW   = ROW + 1
                  MCROW = ROW
                  CALL OTABMC (INTABL, 'READ', MCROW, NUMPOL, NUMIF,
     *                         TIME, SOURID, ANTNUM, SUBARR, FREQID,
     *                         ATMOS, DATMOS, GDELAY, GRATE, CLOCK,
     *                         DCLOCK, LOOFF, DLOOFF, DISP, DDISP, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    Input table row number ROW was read and is not
C                    flagged.
C
                     DO 20 IFNUM = 1, NUMIF
                        NEWOFF(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = LOOFF(1, IFNUM)
                        NEWDOF(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = DLOOFF(1, IFNUM)
   20                CONTINUE
                     IF (STKDIM .EQ. 2) THEN
                        CLOCK(2)  = CLOCK(1)
                        DCLOCK(2) = DCLOCK(1)
                        DISP(2)   = DISP(1)
                        DDISP(2)  = DDISP(1)
                     END IF
C
C                    Either write output table row number OUTROW,
C                    increment OUTROW, and set IRET to 0 or issue one or
C                    more fatal error messages and set IRET to a
C                    non-zero value:
C
                     CALL OTABMC (OUTTBL, 'WRIT', OUTROW, STKDIM,
     *                            IFDIM, TIME, SOURID, ANTNUM, SUBARR,
     *                            FREQID, ATMOS, DATMOS, GDELAY, GRATE,
     *                            CLOCK, DCLOCK, NEWOFF, NEWDOF, DISP,
     *                            DDISP, IRET)
C
                     IF (IRET .NE. 0) THEN
C
C                       Failed to write row.
C
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (9)
                     END IF
                  ELSE IF (IRET .LT. 0) THEN
C
C                    Input table row number ROW was read and is flagged.
C
C                    Clear error indicator:
C
                     IRET = 0
C
                  ELSE
C
C                    Failed to read row.
C
                     WRITE (MSGTXT, 9011) IRET
                     CALL MSGWRT (9)
                  END IF
               GO TO 10
               END IF
C
C              Either close the output table and leave IRET unchanged or
C              issue one or more fatal error messages and set IRET to a
C              non-zero value:
C
               CALL OTABMC (OUTTBL, 'CLOS', OUTROW, STKDIM, IFDIM, TIME,
     *                      SOURID, ANTNUM, SUBARR, FREQID, ATMOS,
     *                      DATMOS, GDELAY, GRATE, CLOCK, DCLOCK,
     *                      NEWOFF, NEWDOF, DISP, DDISP, IRET1)
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 9012) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
C
            ELSE
C
C              Failed to open output table.
C
               WRITE (MSGTXT, 9013) IRET
               CALL MSGWRT (9)
            END IF
C
C           Either close the input table and leave IRET unchanged or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL OTABMC (INTABL, 'CLOS', MCROW, NUMPOL, NUMIF, TIME,
     *                   SOURID, ANTNUM, SUBARR, FREQID, ATMOS, DATMOS,
     *                   GDELAY, GRATE, CLOCK, DCLOCK, LOOFF, DLOOFF,
     *                   DISP, DDISP, IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           Failed to open input table.
C
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPMC3: FAILED TO CLOSE INPUT TABLE (ERROR ', I4, ')')
 9001 FORMAT ('FXPMC3: FAILED TO OPEN INPUT TABLE (ERROR ', I4, ')')
 9010 FORMAT ('FXPMC3: FAILED TO WRITE TO OUTPUT TABLE (ERROR ', I4,
     *        ')')
 9011 FORMAT ('FXPMC3: FAILED TO READ FROM INPUT TABLE (ERROR ', I4,
     *        ')')
 9012 FORMAT ('FXPMC3: FAILED TO CLOSE OUTPUT TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXPMC3: FAILED TO OPEN OUTPUT TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPOB (INFILE, OUTFIL, OBVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy orbit table OBVER.
C
C   If there is an orbit table with version number OBVER attached to
C   INFILE then attach a transformed copy to OUTFIL with version number
C   OBVER and set IRET to zero. If there is no orbit table with version
C   number OBVER attached to INFILE then set IRET to zero. If the table
C   can not be transformed or copied then issue a fatal error message
C   and set IRET to a non-zero value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     OBVER    I        frequency table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   OBVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input OB table
C              (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'OB', OBVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as OB
C           table version number OBVER and set IRET to zero or issue one
C           or more fatal error messages and set IRET to a non-zero
C           value:
C
            CALL FXPOB2 (INTABL, OUTFIL, OBVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPOB: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPOB: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPOB2 (INTABL, OUTFIL, OBVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   OBVER.
C
C   Either attach a copy of the OB table INTABL with IF entries
C   modified to be consistent with the current IF/polarization mapping
C   to OUTFIL as OB table version number OBVER and set IRET to zero
C   or issue one or more fatal error messages and set IRET to a non-zero
C   value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input OB
C                       table: must reference an existing OB table and
C                       must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     OBVER    I        OB table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   OBVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C
C     NKEYS    number of header keywords to modify
C     KEYS     keywords to modify
C     KVALS    keyword value array
C     KLOCS    keyword location index
C     KTYPE    keyword type codes
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
C
      INTEGER   NKEYS
      PARAMETER (NKEYS = 3)
      CHARACTER KEYS(NKEYS)*8
      INTEGER   KVALS(2 * NKEYS)
      INTEGER   KLOCS(NKEYS)
      INTEGER   KTYPE(NKEYS)
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C
      DATA KEYS  / 'NO_STKD ', 'STK_1   ', 'NO_BAND ' /
      DATA KLOCS /  1,          2,          3         /
      DATA KTYPE /  4,          4,          4         /
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'OB', OBVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        The changes in the polarization/IF structure only affect the
C        table header so the table is simply copied to the output file
C        and then the header keywords are corrected.
C
C        Either copy INTABL to OUTTBL and set IRET to 0 or set IRET to
C        a non-zero value if the table can not be copied:
C
         CALL TBLCOP (INTABL, OUTTBL, IRET)
C
         IF (IRET .EQ. 0) THEN
C
C           Either open the output table and set IRET to zero or set
C           IRET to a non-zero value if the table can not be opened:
C
            CALL TABOPN (OUTTBL, 'WRIT', IRET)
C
            IF (IRET .EQ. 0) THEN
C
C              Either set the number of polarizations to STKDIM, set the
C              first polarization to STK1, set the number of IFs to
C              IFDIM, and set IRET to 0 or set IRET to a non-zero value
C              if the table header can not be updated:
C
               KVALS(KLOCS(1)) = STKDIM
               KVALS(KLOCS(2)) = STK1
               KVALS(KLOCS(3)) = IFDIM
               CALL TABKPT (OUTTBL, KEYS, NKEYS, KLOCS, KVALS, KTYPE,
     *                      IRET)
C
               IF (IRET .NE. 0) THEN
                  WRITE (MSGTXT, 9000) IRET
                  CALL MSGWRT (9)
               END IF
C
C              Either close the table and set IRET1 to zero or set IRET1
C              to a non-zero value if the table can not be closed:
C
               CALL TABCLO (OUTTBL, IRET1)
C
               IF (IRET1 .NE. 0) THEN
                  IRET = IRET1
                  WRITE (MSGTXT, 9001) IRET
                  CALL MSGWRT (9)
               END IF
            ELSE
C
C              Open failed.
C
               WRITE (MSGTXT, 9002) IRET
               CALL MSGWRT (9)
            END IF
         ELSE
C
C           Copy failed.
C
            WRITE (MSGTXT, 9003) IRET
            CALL MSGWRT (9)
         END IF
C
C        Either de-allocate the OUTTBL object and set IRET1 to zero or
C        set IRET1 to a non-zero value if the object can not be freed:
C
         CALL TABDES (OUTTBL, IRET1)
C
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9004) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9005) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPOB2: FAILED TO UPDATE KEYWORDS (ERROR ', I4, ')')
 9001 FORMAT ('FXPOB2: FAILED TO CLOSE TABLE (ERROR ', I4, ')')
 9002 FORMAT ('FXPOB2: FAILED TO OPEN TABLE (ERROR ', I4, ')')
 9003 FORMAT ('FXPOB2: FAILED TO COPY TABLE (ERROR ', I4, ')')
 9004 FORMAT ('FXPOB2: FAILED TO FREE TABLE OBJECT (ERROR ', I4, ')')
 9005 FORMAT ('FXPOB2: FAILED TO ALLOCATE TABLE OBJECT (ERROR ', I4,
     *        ')')
      END
      SUBROUTINE FIXPPC (INFILE, OUTFIL, PCVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy phase cal table IMVER.
C
C   If there is a phase cal table with version number PCVER attached to
C   INFILE then attach a transformed copy to OUTFIL with version number
C   PCVER and set IRET to zero. If there is no phase cal table with
C   version number PCVER attached to INFILE then set IRET to zero. If
C   the table can not be transformed or copied then issue a fatal error
C   message and set IRET to a non-zero  value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     PCVER    I        phase cal table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   PCVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input interferometer
C              model table (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'PC', PCVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as
C           interferometer model table version number IMVER and set
C           IRET to zero or issue one or more fatal error messages and
C           set IRET to a non-zero value:
C
            CALL FXPPC2 (INTABL, OUTFIL, PCVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPPC: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPPC: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPPC2 (INTABL, OUTFIL, PCVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   PCVER.
C
C   Either attach a copy of the phase cal table INTABL with IF entries
C   modified to be consistent with the current IF/polarization mapping
C   to OUTFIL as phase cal table version number PCVER and set IRET to
C   zero or issue one or more fatal error messages and set IRET to a
C   non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input IM
C                       table: must reference an existing phase cal
C                       table and must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     PCVER    I        PC table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   PCVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C     EXISTS   does the table corresponding to OUTTBL already exist?
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'PC', PCVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If there is already a table corresponding to OUTTBL then either
C        remove it and set IRET to zero or issue one or more fatal error
C        messages and set IRET to a non-zero value. If there is no table
C        corresponding to OUTTBL then set IRET to 0.
C
         CALL TABEXI (OUTTBL, EXISTS, IRET)
         IRET = 0
         IF (EXISTS) THEN
            CALL TABRMV (OUTTBL, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Announce table being copied:
C
            WRITE (MSGTXT, 4000) PCVER
            CALL MSGWRT (4)
C
C           Either make OUTTBL a transformed copy of INTABL and set IRET
C           to zero or issue one or more fatal error messages and set
C           IRET to a non-zero value:
C
            CALL FXPPC3 (INTABL, OUTTBL, IRET)
C
         END IF
C
C        Either delete the OUTTBL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (OUTTBL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9001) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 4000 FORMAT ('Fixing PC table version ', I4)
 9000 FORMAT ('FXPPC2: TABLE REMOVAL FAILURE (ERROR ', I4, ')')
 9001 FORMAT ('FXPPC2: OBJECT DESTRUCTION FAILURE (ERROR ', I4, ')')
 9002 FORMAT ('FXPPC2: OBJECT ALLOCATION FAILURE (ERROR ', I4, ')')
      END
      SUBROUTINE FXPPC3 (INTABL, OUTTBL, IRET)
C-----------------------------------------------------------------------
C   Make phase cal table OUTTBL a transformed copy of INTABL.
C
C   Either create OUTTBL as a copy of INTABL with the IF entries
C   re-mapped according to the current IF/polarization mapping and set
C   IRET to 0 or issue one or more fatal error messages and set IRET to
C   a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of table object used to access input PC
C                       table: must reference an existing PC table
C                       belonging to a file for which the
C                       IF/polarization mapping is valid
C     OUTTBL   C*(*)    name of table object used to access output PC
C                       table: must not reference an existing table
C
C   Output:
C     IRET     I        return status: 0 if output table created,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTTBL*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PPCV.INC'
C
C     Local variables:
C
C     NUMROW   number of rows in INTABL
C
C     PCROW    IM table row number
C     NUMPOL   number of polarizations in INTABL
C     NUMIF    length of IF axis
C     NUMTON   number of phase cal tones
C
C     ROW      number of last row read from INTABL
C     OUTROW   number of next row to write to OUTTBL
C
C     TIME     time
C     TIMINT   time interval
C     SOURID   source ID
C     ANTNUM   solution antenna number
C     SUBARR   solution subarray number
C     FREQID   solution frequency ID
C     CABCAL   cable cal
C     STATE    state counts by polarization, state, and IF
C     PCFREQ   phase cal frequencies by polarization and IF
C     PCREAL   real parts of phase cal measurements by polarization and
C              IF
C     PCIMAG   imaginary parts of phase cal measurements by polarization
C              and IF
C     PCRATE   phase cal drift rates by polarization and IF
C
C     IFNUM    IF number
C     TONE     tone number
C
C     NEWSTA   remapped state counts
C     NEWFRQ   remapped frequencies
C     NEWREA   remapped real parts
C     NEWIMA   remapped imaginary parts
C     NEWRAT   remapped drift rates
C
C     TYPE     attribute type code
C     DIM      attribute dimensions list
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   NUMROW
C
      INTEGER   PCROW
      INTEGER   NUMPOL
      INTEGER   NUMIF
      INTEGER   NUMTON
C
      INTEGER   ROW
      INTEGER   OUTROW
C
      DOUBLE PRECISION TIME
      REAL             TIMINT
      INTEGER          SOURID
      INTEGER          ANTNUM
      INTEGER          SUBARR
      INTEGER          FREQID
      DOUBLE PRECISION CABCAL
      REAL             STATE(2, 4, MAXIF)
      DOUBLE PRECISION PCFREQ(2, MAXTON, MAXIF)
      REAL             PCREAL(2, MAXTON, MAXIF)
      REAL             PCIMAG(2, MAXTON, MAXIF)
      REAL             PCRATE(2, MAXTON, MAXIF)
C
      INTEGER   IFNUM
      INTEGER   TONE
C
      REAL             NEWSTA(2, 4, MAXIF)
      DOUBLE PRECISION NEWFRQ(2, MAXTON, MAXIF)
      REAL             NEWREA(2, MAXTON, MAXIF)
      REAL             NEWIMA(2, MAXTON, MAXIF)
      REAL             NEWRAT(2, MAXTON, MAXIF)
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     Fill the remapped arrays with null values. Those values in the
C     range of the polarization/IF mapping function will be over-ridden
C     by values from the source PC table later in this routine.
C
      CALL RFILL (2 * 4 * MAXIF, FBLANK, NEWSTA)
      CALL DFILL (2 * MAXTON * MAXIF, DBLANK, NEWFRQ)
      CALL RFILL (2 * MAXTON * MAXIF, FBLANK, NEWREA)
      CALL RFILL (2 * MAXTON * MAXIF, FBLANK, NEWIMA)
      CALL RFILL (2 * MAXTON * MAXIF, FBLANK, NEWRAT)
C-----------------------------------------------------------------------
C
C     A deficiency in the TABLE interface prevents the number of rows
C     being read if the TABLE has not been open with the generic TABOPN
C     interface so the table must opened to read the number of rows and
C     then closed before being opened by the specialized OPCINI
C     interface.
C
C     If INTABL can be opened and closed then set NUMROW to the number
C     of rows in INTABL and IRET to zero otherwise issue one or more
C     fatal error messages and set IRET to a non-zero value:
C
      CALL TABOPN (INTABL, 'READ', IRET)
      IF (IRET .EQ. 0) THEN
C
C        Set NUMROW to the number of rows in INTABL and IRET to 0:
C
         CALL TABGET (INTABL, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
         CALL TABCLO (INTABL, IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Table open failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
      IF (IRET .EQ. 0) THEN
C
C        Either make OUTTBL a copy of INTABL with IF entries remapped
C        according to the current polarization/IF mapping and set IRET
C        to 0 or issue one or more fatal error messages and set IRET to
C        a non-zero value:
C
         CALL OPCINI (INTABL, 'READ', PCROW, NUMPOL, NUMIF, NUMTON,
     *                IRET)
         IF (IRET .EQ. 0) THEN
C
C           Input table was opened for reading.
C
            CALL OPCINI (OUTTBL, 'WRIT', PCROW, STKDIM, IFDIM, NUMTON,
     *                   IRET)
            IF (IRET .EQ. 0) THEN
C
C              Output table was opened for writing.
C
               ROW    = 0
               OUTROW = 1
   10          IF ((IRET .EQ. 0) .AND. (ROW .NE. NUMROW)) THEN
                  ROW   = ROW + 1
                  PCROW = ROW
                  CALL OTABPC (INTABL, 'READ', PCROW, NUMPOL, TIME,
     *                         TIMINT, SOURID, ANTNUM, SUBARR, FREQID,
     *                         CABCAL, STATE, PCFREQ, PCREAL, PCIMAG,
     *                         PCRATE, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    Input table row number ROW was read and is not
C                    flagged.
C
                     DO 40 IFNUM = 1, NUMIF
                        DO 20 TONE = 1, 4
                           NEWSTA(STKIDX(IFNUM), TONE, IFINDX(IFNUM)) =
     *                        STATE(1, TONE, IFNUM)
   20                   CONTINUE
                        DO 30 TONE = 1, NUMTON
                           NEWFRQ(STKIDX(IFNUM), TONE, IFINDX(IFNUM)) =
     *                        PCFREQ(1, TONE, IFNUM)
                           NEWREA(STKIDX(IFNUM), TONE, IFINDX(IFNUM)) =
     *                        PCREAL(1, TONE, IFNUM)
                           NEWIMA(STKIDX(IFNUM), TONE, IFINDX(IFNUM)) =
     *                        PCIMAG(1, TONE, IFNUM)
                           NEWRAT(STKIDX(IFNUM), TONE, IFINDX(IFNUM)) =
     *                        PCRATE(1, TONE, IFNUM)
   30                   CONTINUE
   40                CONTINUE
C
C                    Either write output table row number OUTROW,
C                    increment OUTROW, and set IRET to 0 or issue one or
C                    more fatal error messages and set IRET to a
C                    non-zero value:
C
                     CALL OTABPC (OUTTBL, 'WRIT', OUTROW, STKDIM,
     *                            TIME, TIMINT, SOURID, ANTNUM, SUBARR,
     *                            FREQID, CABCAL, NEWSTA, NEWFRQ,
     *                            NEWREA, NEWIMA, NEWRAT, IRET)
C
                     IF (IRET .NE. 0) THEN
C
C                       Failed to write row.
C
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (9)
                     END IF
                  ELSE IF (IRET .LT. 0) THEN
C
C                    Input table row number ROW was read and is flagged.
C
C                    Clear error indicator:
C
                     IRET = 0
C
                  ELSE
C
C                    Failed to read row.
C
                     WRITE (MSGTXT, 9011) IRET
                     CALL MSGWRT (9)
                  END IF
               GO TO 10
               END IF
C
C              Either close the output table and leave IRET unchanged or
C              issue one or more fatal error messages and set IRET to a
C              non-zero value:
C
               CALL OTABPC (OUTTBL, 'CLOS', OUTROW, STKDIM, TIME,
     *                      TIMINT, SOURID, ANTNUM, SUBARR, FREQID,
     *                      CABCAL, NEWSTA, NEWFRQ, NEWREA, NEWIMA,
     *                      NEWRAT, IRET1)
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 9012) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
C
            ELSE
C
C              Failed to open output table.
C
               WRITE (MSGTXT, 9013) IRET
               CALL MSGWRT (9)
            END IF
C
C           Either close the input table and leave IRET unchanged or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL OTABPC (INTABL, 'CLOS', PCROW, NUMPOL, TIME, TIMINT,
     *                   SOURID, ANTNUM, SUBARR, FREQID, CABCAL, STATE,
     *                   PCFREQ, PCREAL, PCIMAG, PCRATE, IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           Failed to open input table.
C
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPPC3: FAILED TO CLOSE INPUT TABLE (ERROR ', I4, ')')
 9001 FORMAT ('FXPPC3: FAILED TO OPEN INPUT TABLE (ERROR ', I4, ')')
 9010 FORMAT ('FXPPC3: FAILED TO WRITE TO OUTPUT TABLE (ERROR ', I4,
     *        ')')
 9011 FORMAT ('FXPPC3: FAILED TO READ FROM INPUT TABLE (ERROR ', I4,
     *        ')')
 9012 FORMAT ('FXPPC3: FAILED TO CLOSE OUTPUT TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXPPC3: FAILED TO OPEN OUTPUT TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPTY (INFILE, OUTFIL, TYVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy system temperature table TYVER.
C
C   If there is a system temperature table with version number TYVER
C   attached to INFILE then attach a transformed copy to OUTFIL with
C   version number TYVER and set IRET to zero. If there is no
C   system temperature table with version number TYVER attached to
C   INFILE then set IRET to zero. If the table can not be transformed
C   or copied then issue a fatal error message and set IRET to a non-zero
C   value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     TYVER    I        system temperature table version number: must be
C                       positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   TYVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input system
C              temperature table (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'TY', TYVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as
C           system termperature table version number TYVER and set
C           IRET to zero or issue one or more fatal error messages and
C           set IRET to a non-zero value:
C
            CALL FXPTY2 (INTABL, OUTFIL, TYVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPTY: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPTY: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPTY2 (INTABL, OUTFIL, TYVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   TYVER.
C
C   Either attach a copy of the system temperature table INTABL with
C   IF entries modified to be consistent with the current
C   IF/polarization mapping to OUTFIL as system temperature table
C   version number TYVER and set IRET to zero or issue one or more fatal
C   error messages and set IRET to a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input TY
C                       table: must reference an existing model
C                       components table and must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     TYVER    I        TY table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   TYVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C     EXISTS   does the table corresponding to OUTTBL already exist?
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'TY', TYVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        If there is already a table corresponding to OUTTBL then either
C        remove it and set IRET to zero or issue one or more fatal error
C        messages and set IRET to a non-zero value. If there is no table
C        corresponding to OUTTBL then set IRET to 0.
C
         CALL TABEXI (OUTTBL, EXISTS, IRET)
         IRET = 0
         IF (EXISTS) THEN
            CALL TABRMV (OUTTBL, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
            END IF
         END IF
C
         IF (IRET .EQ. 0) THEN
C
C           Announce table being copied:
C
            WRITE (MSGTXT, 4000) TYVER
            CALL MSGWRT (4)
C
C           Either make OUTTBL a transformed copy of INTABL and set IRET
C           to zero or issue one or more fatal error messages and set
C           IRET to a non-zero value:
C
            CALL FXPTY3 (INTABL, OUTTBL, IRET)
C
         END IF
C
C        Either delete the OUTTBL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (OUTTBL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9001) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 4000 FORMAT ('Fixing TY table version ', I4)
 9000 FORMAT ('FXPTY2: TABLE REMOVAL FAILURE (ERROR ', I4, ')')
 9001 FORMAT ('FXPTY2: OBJECT DESTRUCTION FAILURE (ERROR ', I4, ')')
 9002 FORMAT ('FXPTY2: OBJECT ALLOCATION FAILURE (ERROR ', I4, ')')
      END
      SUBROUTINE FXPTY3 (INTABL, OUTTBL, IRET)
C-----------------------------------------------------------------------
C   Make system temperature table OUTTBL a transformed copy of INTABL.
C
C   Either create OUTTBL as a copy of INTABL with the IF entries
C   re-mapped according to the current IF/polarization mapping and set
C   IRET to 0 or issue one or more fatal error messages and set IRET to
C   a non-zero value.
C
C   Inputs:
C     INTABL   C*(*)    name of table object used to access input TY
C                       table: must reference an existing TY table
C                       belonging to a file for which the
C                       IF/polarization mapping is valid
C     OUTTBL   C*(*)    name of table object used to access output TY
C                       table: must not reference an existing table
C
C   Output:
C     IRET     I        return status: 0 if output table created,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTTBL*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     NUMROW   number of rows in INTABL
C
C     TYROW    TY table row number
C     NUMPOL   number of polarizations in INTABL
C     NUMIF    length of IF axis
C
C     ROW      number of last row read from INTABL
C     OUTROW   number of next row to write to OUTTBL
C
C     TIME     time
C     TIMINT   time interval
C     SOURID   source ID
C     ANTNUM   solution antenna number
C     SUBARR   solution subarray number
C     FREQID   solution frequency ID
C     TSYS     system temperatures by polarization and IF
C     TANT     antenna temperatures by polarization and IF
C
C     IFNUM    IF number
C
C     NEWTS    remapped system temperatures
C     NEWTA    remapped antenna temperatures
C
C     TYPE     attribute type code
C     DIM      attribute dimensions list
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   NUMROW
C
      INTEGER   TYROW
      INTEGER   NUMPOL
      INTEGER   NUMIF
C
      INTEGER   ROW
      INTEGER   OUTROW
C
      REAL      TIME
      REAL      TIMINT
      INTEGER   SOURID
      INTEGER   ANTNUM
      INTEGER   SUBARR
      INTEGER   FREQID
      REAL      TSYS(2, MAXIF)
      REAL      TANT(2, MAXIF)
C
      INTEGER   IFNUM
C
      REAL      NEWTS(2, MAXIF)
      REAL      NEWTA(2, MAXIF)
C
      INTEGER   TYPE
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C-----------------------------------------------------------------------
C
C     Fill the remapped arrays with null values. Those values in the
C     range of the polarization/IF mapping function will be over-ridden
C     by values from the source IM table later in this routine.
C
      CALL RFILL (2 * MAXIF, FBLANK, NEWTS)
      CALL RFILL (2 * MAXIF, FBLANK, NEWTA)
C-----------------------------------------------------------------------
C
C     A deficiency in the TABLE interface prevents the number of rows
C     being read if the TABLE has not been open with the generic TABOPN
C     interface so the table must opened to read the number of rows and
C     then closed before being opened by the specialized OTYINI
C     interface.
C
C     If INTABL can be opened and closed then set NUMROW to the number
C     of rows in INTABL and IRET to zero otherwise issue one or more
C     fatal error messages and set IRET to a non-zero value:
C
      CALL TABOPN (INTABL, 'READ', IRET)
      IF (IRET .EQ. 0) THEN
C
C        Set NUMROW to the number of rows in INTABL and IRET to 0:
C
         CALL TABGET (INTABL, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
C
         CALL TABCLO (INTABL, IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
         END IF
      ELSE
C
C        Table open failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
      IF (IRET .EQ. 0) THEN
C
C        Either make OUTTBL a copy of INTABL with IF entries remapped
C        according to the current polarization/IF mapping and set IRET
C        to 0 or issue one or more fatal error messages and set IRET to
C        a non-zero value:
C
         CALL OTYINI (INTABL, 'READ', TYROW, NUMPOL, NUMIF, IRET)
         IF (IRET .EQ. 0) THEN
C
C           Input table was opened for reading.
C
            CALL OTYINI (OUTTBL, 'WRIT', TYROW, STKDIM, IFDIM, IRET)
            IF (IRET .EQ. 0) THEN
C
C              Output table was opened for writing.
C
               ROW    = 0
               OUTROW = 1
   10          IF ((IRET .EQ. 0) .AND. (ROW .NE. NUMROW)) THEN
                  ROW   = ROW + 1
                  TYROW = ROW
                  CALL OTABTY (INTABL, 'READ', TYROW, NUMPOL, NUMIF,
     *                         TIME, TIMINT, SOURID, ANTNUM, SUBARR,
     *                         FREQID, TSYS, TANT, IRET)
                  IF (IRET .EQ. 0) THEN
C
C                    Input table row number ROW was read and is not
C                    flagged.
C
                     DO 20 IFNUM = 1, NUMIF
                        NEWTS(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = TSYS(1, IFNUM)
                        NEWTA(STKIDX(IFNUM), IFINDX(IFNUM))
     *                     = TANT(1, IFNUM)
   20                CONTINUE
C
C                    Either write output table row number OUTROW,
C                    increment OUTROW, and set IRET to 0 or issue one or
C                    more fatal error messages and set IRET to a
C                    non-zero value:
C
                     CALL OTABTY (OUTTBL, 'WRIT', OUTROW, STKDIM,
     *                            IFDIM, TIME, TIMINT, SOURID, ANTNUM,
     *                            SUBARR, FREQID, NEWTS, NEWTA, IRET)
C
                     IF (IRET .NE. 0) THEN
C
C                       Failed to write row.
C
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (9)
                     END IF
                  ELSE IF (IRET .LT. 0) THEN
C
C                    Input table row number ROW was read and is flagged.
C
C                    Clear error indicator:
C
                     IRET = 0
C
                  ELSE
C
C                    Failed to read row.
C
                     WRITE (MSGTXT, 9011) IRET
                     CALL MSGWRT (9)
                  END IF
               GO TO 10
               END IF
C
C              Either close the output table and leave IRET unchanged or
C              issue one or more fatal error messages and set IRET to a
C              non-zero value:
C
               CALL OTABTY (OUTTBL, 'CLOS', OUTROW, STKDIM, IFDIM, TIME,
     *                      TIMINT, SOURID, ANTNUM, SUBARR, FREQID,
     *                      NEWTS, NEWTA, IRET1)
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 9012) IRET1
                  CALL MSGWRT (9)
                  IRET = IRET1
               END IF
C
            ELSE
C
C              Failed to open output table.
C
               WRITE (MSGTXT, 9013) IRET
               CALL MSGWRT (9)
            END IF
C
C           Either close the input table and leave IRET unchanged or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL OTABTY (INTABL, 'CLOS', TYROW, NUMPOL, NUMIF, TIME,
     *                   TIMINT, SOURID, ANTNUM, SUBARR, FREQID, TSYS,
     *                   TANT, IRET1)
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 9000) IRET1
               CALL MSGWRT (9)
               IRET = IRET1
            END IF
C
         ELSE
C
C           Failed to open input table.
C
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
         END IF
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPTY3: FAILED TO CLOSE INPUT TABLE (ERROR ', I4, ')')
 9001 FORMAT ('FXPTY3: FAILED TO OPEN INPUT TABLE (ERROR ', I4, ')')
 9010 FORMAT ('FXPTY3: FAILED TO WRITE TO OUTPUT TABLE (ERROR ', I4,
     *        ')')
 9011 FORMAT ('FXPTY3: FAILED TO READ FROM INPUT TABLE (ERROR ', I4,
     *        ')')
 9012 FORMAT ('FXPTY3: FAILED TO CLOSE OUTPUT TABLE (ERROR ', I4, ')')
 9013 FORMAT ('FXPTY3: FAILED TO OPEN OUTPUT TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE FIXPVT (INFILE, OUTFIL, VTVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy tape statistics table VTVER.
C
C   If there is a tape statistics table with version number VTVER
C   attached to INFILE then attach a transformed copy to OUTFIL with
C   version number VTVER and set IRET to zero. If there is no
C   tape statistics table with version number VTVER attached to
C   INFILE then set IRET to zero. If the table can not be transformed
C   or copied then issue a fatal error message and set IRET to a non-zero
C   value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     VTVER    I        tape statistics table version number: must be
C                       positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   VTVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input tape
C              statistics table (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'VT', VTVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as
C           tape statistics table version number VTVER and set
C           IRET to zero or issue one or more fatal error messages and
C           set IRET to a non-zero value:
C
            CALL FXPVT2 (INTABL, OUTFIL, VTVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPVT: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPVT: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPVT2 (INTABL, OUTFIL, VTVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of INTABL to OUTFIL with version number
C   VTVER.
C
C   Either attach a copy of the VT table INTABL with IF entries
C   modified to be consistent with the current IF/polarization mapping
C   to OUTFIL as VT table version number VTVER and set IRET to zero
C   or issue one or more fatal error messages and set IRET to a non-zero
C   value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input VT
C                       table: must reference an existing VT table and
C                       must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     VTVER    I        VT table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   VTVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C
C     NKEYS    number of header keywords to modify
C     KEYS     keywords to modify
C     KVALS    keyword value array
C     KLOCS    keyword location index
C     KTYPE    keyword type codes
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
C
      INTEGER   NKEYS
      PARAMETER (NKEYS = 3)
      CHARACTER KEYS(NKEYS)*8
      INTEGER   KVALS(2 * NKEYS)
      INTEGER   KLOCS(NKEYS)
      INTEGER   KTYPE(NKEYS)
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C
      DATA KEYS  / 'NO_STKD ', 'STK_1   ', 'NO_BAND ' /
      DATA KLOCS /  1,          2,          3         /
      DATA KTYPE /  4,          4,          4         /
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'VT', VTVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        The changes in the polarization/IF structure only affect the
C        table header so the table is simply copied to the output file
C        and then the header keywords are corrected.
C
C        Either copy INTABL to OUTTBL and set IRET to 0 or set IRET to
C        a non-zero value if the table can not be copied:
C
         CALL TBLCOP (INTABL, OUTTBL, IRET)
C
         IF (IRET .EQ. 0) THEN
C
C           Either open the output table and set IRET to zero or set
C           IRET to a non-zero value if the table can not be opened:
C
            CALL TABOPN (OUTTBL, 'WRIT', IRET)
C
            IF (IRET .EQ. 0) THEN
C
C              Either set the number of polarizations to STKDIM, set the
C              first polarization to STK1, set the number of IFs to
C              IFDIM, and set IRET to 0 or set IRET to a non-zero value
C              if the table header can not be updated:
C
               KVALS(KLOCS(1)) = STKDIM
               KVALS(KLOCS(2)) = STK1
               KVALS(KLOCS(3)) = IFDIM
               CALL TABKPT (OUTTBL, KEYS, NKEYS, KLOCS, KVALS, KTYPE,
     *                      IRET)
C
               IF (IRET .NE. 0) THEN
                  WRITE (MSGTXT, 9000) IRET
                  CALL MSGWRT (9)
               END IF
C
C              Either close the table and set IRET1 to zero or set IRET1
C              to a non-zero value if the table can not be closed:
C
               CALL TABCLO (OUTTBL, IRET1)
C
               IF (IRET1 .NE. 0) THEN
                  IRET = IRET1
                  WRITE (MSGTXT, 9001) IRET
                  CALL MSGWRT (9)
               END IF
            ELSE
C
C              Open failed.
C
               WRITE (MSGTXT, 9002) IRET
               CALL MSGWRT (9)
            END IF
         ELSE
C
C           Copy failed.
C
            WRITE (MSGTXT, 9003) IRET
            CALL MSGWRT (9)
         END IF
C
C        Either de-allocate the OUTTBL object and set IRET1 to zero or
C        set IRET1 to a non-zero value if the object can not be freed:
C
         CALL TABDES (OUTTBL, IRET1)
C
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9004) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9005) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPVT2: FAILED TO UPDATE KEYWORDS (ERROR ', I4, ')')
 9001 FORMAT ('FXPVT2: FAILED TO CLOSE TABLE (ERROR ', I4, ')')
 9002 FORMAT ('FXPVT2: FAILED TO OPEN TABLE (ERROR ', I4, ')')
 9003 FORMAT ('FXPVT2: FAILED TO COPY TABLE (ERROR ', I4, ')')
 9004 FORMAT ('FXPVT2: FAILED TO FREE TABLE OBJECT (ERROR ', I4, ')')
 9005 FORMAT ('FXPVT2: FAILED TO ALLOCATE TABLE OBJECT (ERROR ', I4,
     *        ')')
      END
      SUBROUTINE FIXPWX (INFILE, OUTFIL, WXVER, IRET)
C-----------------------------------------------------------------------
C   Reformat and copy weather table WXVER.
C
C   If there is a weather table with version number WXVER attached to
C   INFILE then attach a transformed copy to OUTFIL with version number
C   WXVER and set IRET to zero. If there is no weather table with
C   version number WXVER attached to INFILE then set IRET to zero. If
C   the table can not be transformed or copied then issue a fatal error
C   message and set IRET to a non-zero value.
C
C   Inputs:
C     INFILE   C*(*)    name of UVDATA object used to access input file
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file: must not reference the same file as INFILE
C     WXVER    I        weather table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed successfully
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   WXVER
      INTEGER   IRET
C
C     Local variables:
C
C     INTABL   name of TABLE object used to access input weather table
C              (constant)
C     EXISTS   is there an actual table corresponding to INTABL?
C
C     IRET1    alternate return status
C
      CHARACTER INTABL*11
      PARAMETER (INTABL = 'Input table')
      LOGICAL   EXISTS
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Either create the input table object and set IRET to 0 or set IRET
C     to a non-zero value:
C
      CALL UV2TAB (INFILE, INTABL, 'WX', WXVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        Establish the definition of EXISTS and set IRET to 0:
C
         CALL TABEXI (INTABL, EXISTS, IRET)
         IRET = 0
C
         IF (EXISTS) THEN
C
C           Either attach a transformed copy of INTABL to OUTFIL as
C           weather table version number WXVER and set IRET to zero or
C           issue one or more fatal error messages and set IRET to a
C           non-zero value:
C
            CALL FXPWX2 (INTABL, OUTFIL, WXVER, IRET)
C
         END IF
C
C        Either delete the INTABL object and leave IRET unchanged or
C        issue one or more fatal error messages and set IRET to a
C        non-zero value:
C
         CALL TABDES (INTABL, IRET1)
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FIXPWX: OBJECT DESTRUCTION FAILED (ERROR ', I4, ')')
 9001 FORMAT ('FIXPWX: OBJECT ALLOCATION FAILED (ERROR ', I4, ')')
      END
      SUBROUTINE FXPWX2 (INTABL, OUTFIL, WXVER, IRET)
C-----------------------------------------------------------------------
C   Attach reformatted version of weather table INTABL to OUTFIL with
C   version number WXVER.
C
C   Either attach a copy of the WX table INTABL with IF entries
C   modified to be consistent with the current IF/polarization mapping
C   to OUTFIL as WX table version number WXVER and set IRET to zero
C   or issue one or more fatal error messages and set IRET to a non-zero
C   value.
C
C   Inputs:
C     INTABL   C*(*)    name of TABLE object used to access input WX
C                       table: must reference an existing WX table and
C                       must not be open.
C     OUTFIL   C*(*)    name of UVDATA object used to access output
C                       file
C     WXVER    I        WX table version number: must be positive
C
C   Output:
C     IRET     I        return status: 0 if table processed completely,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER INTABL*(*)
      CHARACTER OUTFIL*(*)
      INTEGER   WXVER
      INTEGER   IRET
C
C     Local variables:
C
C     OUTTBL   TABLE object used to access output table
C
C     NKEYS    number of header keywords to modify
C     KEYS     keywords to modify
C     KVALS    keyword value array
C     KLOCS    keyword location index
C     KTYPE    keyword type codes
C
C     IRET1    alternate return status
C
      CHARACTER OUTTBL*12
      PARAMETER (OUTTBL = 'Output table')
C
      INTEGER   NKEYS
      PARAMETER (NKEYS = 3)
      CHARACTER KEYS(NKEYS)*8
      INTEGER   KVALS(2 * NKEYS)
      INTEGER   KLOCS(NKEYS)
      INTEGER   KTYPE(NKEYS)
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FXMAPS.INC'
C
      DATA KEYS  / 'NO_STKD ', 'STK_1   ', 'NO_BAND ' /
      DATA KLOCS /  1,          2,          3         /
      DATA KTYPE /  4,          4,          4         /
C-----------------------------------------------------------------------
C
C     Either create object OUTTBL and set IRET to zero or set IRET to a
C     non-zero value:
C
      CALL UV2TAB (OUTFIL, OUTTBL, 'WX', WXVER, IRET)
C
      IF (IRET .EQ. 0) THEN
C
C        The changes in the polarization/IF structure only affect the
C        table header so the table is simply copied to the output file
C        and then the header keywords are corrected.
C
C        Either copy INTABL to OUTTBL and set IRET to 0 or set IRET to
C        a non-zero value if the table can not be copied:
C
         CALL TBLCOP (INTABL, OUTTBL, IRET)
C
         IF (IRET .EQ. 0) THEN
C
C           Either open the output table and set IRET to zero or set
C           IRET to a non-zero value if the table can not be opened:
C
            CALL TABOPN (OUTTBL, 'WRIT', IRET)
C
            IF (IRET .EQ. 0) THEN
C
C              Either set the number of polarizations to STKDIM, set the
C              first polarization to STK1, set the number of IFs to
C              IFDIM, and set IRET to 0 or set IRET to a non-zero value
C              if the table header can not be updated:
C
               KVALS(KLOCS(1)) = STKDIM
               KVALS(KLOCS(2)) = STK1
               KVALS(KLOCS(3)) = IFDIM
               CALL TABKPT (OUTTBL, KEYS, NKEYS, KLOCS, KVALS, KTYPE,
     *                      IRET)
C
               IF (IRET .NE. 0) THEN
                  WRITE (MSGTXT, 9000) IRET
                  CALL MSGWRT (9)
               END IF
C
C              Either close the table and set IRET1 to zero or set IRET1
C              to a non-zero value if the table can not be closed:
C
               CALL TABCLO (OUTTBL, IRET1)
C
               IF (IRET1 .NE. 0) THEN
                  IRET = IRET1
                  WRITE (MSGTXT, 9001) IRET
                  CALL MSGWRT (9)
               END IF
            ELSE
C
C              Open failed.
C
               WRITE (MSGTXT, 9002) IRET
               CALL MSGWRT (9)
            END IF
         ELSE
C
C           Copy failed.
C
            WRITE (MSGTXT, 9003) IRET
            CALL MSGWRT (9)
         END IF
C
C        Either de-allocate the OUTTBL object and set IRET1 to zero or
C        set IRET1 to a non-zero value if the object can not be freed:
C
         CALL TABDES (OUTTBL, IRET1)
C
         IF (IRET1 .NE. 0) THEN
            WRITE (MSGTXT, 9004) IRET1
            IRET = IRET1
            CALL MSGWRT (9)
         END IF
C
      ELSE
C
C        Object allocation failed.
C
         WRITE (MSGTXT, 9005) IRET
         CALL MSGWRT (9)
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('FXPWX2: FAILED TO UPDATE KEYWORDS (ERROR ', I4, ')')
 9001 FORMAT ('FXPWX2: FAILED TO CLOSE TABLE (ERROR ', I4, ')')
 9002 FORMAT ('FXPWX2: FAILED TO OPEN TABLE (ERROR ', I4, ')')
 9003 FORMAT ('FXPWX2: FAILED TO COPY TABLE (ERROR ', I4, ')')
 9004 FORMAT ('FXPWX2: FAILED TO FREE TABLE OBJECT (ERROR ', I4, ')')
 9005 FORMAT ('FXPWX2: FAILED TO ALLOCATE TABLE OBJECT (ERROR ', I4,
     *        ')')
      END
