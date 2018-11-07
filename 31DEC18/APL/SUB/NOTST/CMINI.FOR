      SUBROUTINE CMINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IERR)
C-----------------------------------------------------------------------
C! creates and intializes a CALC model table
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2006, 2009
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
C   Creates and initializes CALC model tables.
C   Inputs:
C      OPCODE         C*4  Operation code:
C                          'WRIT' = create/init for write or read
C                          'READ' = open for read only
C      BUFFER(512)    I    I/O buffer and related storage, also defines
C                          file if open.
C      DISK           I    Disk to use.
C      CNO            I    Catalog slot number
C      CATBLK(256)    I    Catalog header block.
C      LUN            I    Logical unit number to use
C   In/out:
C      VER            I    CM file version
C   Input (create) / output (pre-existing) via common(file keywords):
C      NOPOLZ         I    # polzns in the data
C      NPOLY          I    Order of polynomial used to form geometric
C                          model.
C      REVNUM         R    Revision # of correlator software used to
C                          generate correlator model.
C      OBSCODE        C*8  Observing code
C      NOSTKD         I    No polzns in the data
C      STK1           I    First Stokes parameter in the data
C      NOBAND         I    The number of bands (IF's) in the data.
C      NOCHAN         I    The number of spectral channels in the data.
C      REFFRQ         D    Freq. at reference pixel (Hz)
C      CHNBW          R    Bandwidth of single spectral channel (Hz)
C      REFPIX         R    Reference pixel
C      TABREV         I    Table revision number.
C                          Revision 1 - table invented, PJD, Feb 1, 1991
C      CMNUMV(MAXCMC) I    Element count in each column. On input only
C                          used if the file is created.
C   Output:
C      ICMRNO         I    Next row number, start of the file if READ,
C                          the last+1 if WRITE
C      CMKOLS(MAXCMC) I    The column pointer array in order:
C                          TIME, SOURCE ID, ANTENNA NO.,
C                          ARRAY, FREQID, ATMOS, DATMOS,
C                          GDELAY, CLOCK1, DCLOCK1, DISP1, DDISP1,
C                          the following are only present if NOPOLZ = 2
C                          CLOCK2, DCLOCK2, DISP2, DDISP2
C      IERR           I    Return error code, 0=>OK, else TABINI or
C                          TABIO error.
C----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
C                                       Input variables
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, IERR
C                                       Local variables
      INTEGER   I, ITEMP(6), JERR, NTT, ITRIM, NC
      LOGICAL   T, DOREAD, NEWFIL
      HOLLERITH HOLTMP(6)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (HOLTMP, ITEMP)
      INCLUDE 'INCS:DCMV.INC'
      CHARACTER  KEYW(NKEYCM)*8, TITLE(MAXCMC)*24, UNITS(MAXCMC)*8,
     *   TTITLE*56
C                                       CM and generic data statements
      DATA NTT /56/
      DATA TTITLE /'CALC MODEL TABLE'/
C                                       Table column titles
      DATA TITLE /'TIME', 'SOURCE_ID', 'ANTENNA_NO',
     *   'ARRAY', 'FREQID', 'ATMOS', 'DATMOS',
     *   'GDELAY', 'CLOCK 1', 'DCLOCK 1', 'DISP 1', 'DDISP 1',
     *   'CLOCK 2', 'DCLOCK 2', 'DISP 2', 'DDISP 2' /
C                                       Units of table columns
      DATA UNITS /'DAYS', 4*' ', 'SECONDS', 'SEC/SEC',
     *   'SECONDS', 'SECONDS', 'SEC/SEC', 'SEC/M**2', 'S/S/M**2',
     *   'SECONDS', 'SEC/SEC ', 'SEC/M**2', 'S/S/M**2' /
C                                       Header keywords
      DATA KEYW /'OBSCODE ', 'NO_STKD ', 'STK_1   ', 'NO_BAND ',
     *   'NO_CHAN', 'REF_FREQ', 'CHAN_BW ', 'REF_PIXL',
     *   'NO_POL', 'NPOLY', 'REVISION', 'TABREV' /
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Set up needed variables
      NREC = 30
      NCOL = MAXCMC
      NKEY = NKEYCM
C                                       Fill in types, lengths
C                                       See Going AIPS Vol 2 p13-3.
      IF (.NOT.DOREAD) THEN
         DTYP(KTIM)   = TABDBL + 10
         DTYP(KSOU)   = TABINT + 10
         DTYP(KANO)   = TABINT + 10
         DTYP(KARR)   = TABINT + 10
         DTYP(KFRE)   = TABINT + 10
         DTYP(KATM)   = TABFLT + 10
         DTYP(KDATM)  = TABFLT + 10
         DTYP(KGDL)   = TABDBL + 10*NPOLY
         DTYP(KCLK1)  = TABFLT + 10
         DTYP(KDCLK1) = TABFLT + 10
         DTYP(KDSP1)  = TABFLT + 10
         DTYP(KDDSP1) = TABFLT + 10
         DTYP(KCLK2)  = TABFLT + 10
         DTYP(KDCLK2) = TABFLT + 10
         DTYP(KDSP2)  = TABFLT + 10
         DTYP(KDDSP2) = TABFLT + 10
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'CM', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CMINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      ICMRNO = BUFFER(5) + 1
      IF (DOREAD) ICMRNO = 1
      NKEY = NKEYCM
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CMINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CMINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
         CALL CMIO ('WRIT', KEYVAL, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('WRIT', 'CMIO', 'CMINI', IERR)
            GO TO 990
            END IF
C                                       Write if just created
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR (OPCODE, 'TABKEY', 'CMINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'CMINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
         CALL CMIO ('READ', KEYVAL, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('READ', 'CMIO', 'CMINI', IERR)
            GO TO 990
            END IF
         END IF
      IERR = 0
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CMINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'CM', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CMINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXCMC, TITLE, 24, T, BUFFER, CMKOLS, JERR)
C                                       Get array indices and no. values
      DO 150 I = 1,MAXCMC
         IPOINT = CMKOLS(I)
         IF (IPOINT.GT.0) THEN
            CMKOLS(I) = DATP(IPOINT,1)
            CMNUMV(I) = DATP(IPOINT,2) / 10
            IF (CMNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            CMKOLS(I) = -1
            CMNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('CMINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('CMINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('CMINI: ERROR INITIALIZING CALC MODEL TABLE FOR ',A4)
      END

