LOCAL INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXVS
C                                       MXVS = maximum no. correlations
C                                       in a record.
      PARAMETER (MXVS = MAXCIF)
C                                       Local Info for uv util.
      REAL     RP(50), VS(3,MXVS)
      COMMON /UVULCM/ RP, VS
LOCAL END
LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
      PARAMETER (NPARMS=11)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                                       Uses PAOOF.INC
C                      1        2          3         4        5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'OUTNAME',
C            6           7          8         9          10
     *   'OUTCLASS', 'OUTDISK', 'OUTSEQ', 'NPOINTS', 'INTPARM',
C            11
     *   'BADDISK'/
C                    1       2       3       4       5       6
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR, OOACAR,
C           7       8      9       10      11
     *   OOAINT, OOAINT, OOAINT, OOARE,  OOAINT/
C                   1    2    3    4    5    6    7    8     9    10
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 12,1, 6,1, 1,1, 1,1, 1,1, 3,1,
C          11
     *   10,1/
LOCAL END
LOCAL INCLUDE 'REGRID.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER MAXCHN, NCHAN
      PARAMETER (MAXCHN = MAXCHA)
      COMMON /RGRID/ NCHAN
LOCAL END
      PROGRAM SPECR
C-----------------------------------------------------------------------
C! Spectral regridding task for UV data
C# UV OOP SPECTRAL
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000-2001, 2012-2015
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
C   Spectral regridding task for UV data
C   INNAME                             Input UV data (name)
C   INCLASS                            Input UV data (class)
C   INSEQ                              Input UV data (seq. #)
C   INDISK                             Input UV data disk drive #
C   OUTNAME                            Output uvdata name (name)
C   OUTCLASS                           Output uv data class
C   OUTDISK                            Output uvdata disk drive #
C   OUTSEQ          -1.0    32000.0    Output seq. no.
C   NPOINTS                            (1) # channels in o/p data
C   BADDISK                            Disk drive #'s to avoid
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVIN*36, UVOUT*36
      INTEGER   IRET, BUFF(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'SPECR '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL SPECIN (PRGM, UVIN, UVOUT, IRET)
C                                       Regrid
      IF (IRET.EQ.0) CALL REGRID (UVIN, UVOUT, IRET)
C                                       History
      IF (IRET.EQ.0) CALL SPCHIS (UVIN, UVOUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF)
C
 999  STOP
      END
      SUBROUTINE SPECIN (PRGN, UVIN, UVOUT, IRET)
C-----------------------------------------------------------------------
C   SPECIN gets input parameters for SPECR and creates the input and
C   output objects
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      UVIN    C*?  Input uv data object
C      UVOUT   C*?  Output uv data object
C      IRET    I    Error code: 0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, UVIN*(*), UVOUT*(*)
C
      INTEGER   NKEY1
C                                       NKEY1=no. adverbs to copy to
C                                       UVIN
      PARAMETER (NKEY1=8)
      INTEGER   IERR, DIM(7), TYPE, INTYPE
      REAL      INTPRM(3), INTDEF(6,2)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'REGRID.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to uv data
C                    1         2         3         4         5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'OUTNAME',
C            6           7         8
     *   'OUTCLASS', 'OUTDISK', 'OUTSEQ'/
C                                       Rename to object
C                   1        2        3      4         5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'OUTNAME',
C            6           7          8
     *   'OUTCLASS',  'OUTDISK', 'OUTSEQ'/
C                                       defaults
      DATA INTDEF /4.,2.,2.,3.,1.99,4., 1.,3.,1.,4.,3.,1./
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Create input uv data object
      UVIN = 'Input uv data'
      CALL CREATE (UVIN, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVIN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Output
      UVOUT = 'Output regridded uv data'
C                                       # channels in o/p data
      CALL OGET ('Input', 'NPOINTS', TYPE, DIM, NCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NCHAN.GT.MAXCHN) NCHAN = MAXCHN
      CALL OPUT ('Input', 'NPOINTS', OOAINT, DIM, NCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVIN, 'OPCHANS', OOAINT, DIM, NCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       interpolation method/parms
      CALL OGET ('Input', 'INTPARM', TYPE, DIM, INTPRM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      INTYPE = INTPRM(1) + 0.1
      INTYPE = MAX (0, MIN (7, INTYPE))
      INTPRM(1) = INTYPE
      IF ((INTYPE.GE.1) .AND. (INTYPE.LE.6)) THEN
         IF (INTPRM(2).LT.0.1) INTPRM(2) = INTDEF(INTYPE,1)
         IF (INTPRM(3).LT.INTPRM(2)) INTPRM(3) = INTPRM(2) *
     *      INTDEF(INTYPE,2)
      ELSE
         INTPRM(2) = 0.0
         INTPRM(3) = 0.0
         END IF
      CALL OPUT ('Input', 'INTPARM', TYPE, DIM, INTPRM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVIN, 'INTPARM', TYPE, DIM, INTPRM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE SPCHIS (UVIN, UVOUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output image object.
C   Inputs:
C      UVIN    C*?  UV data object
C      UVOUT   C*?  output image object
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=5)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'NPOINTS'/
C-----------------------------------------------------------------------
C                                        Copy old history
      CALL OHCOPY (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST ('Input', LIST, NADV, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // UVOUT
      CALL MSGWRT (4)
 999  RETURN
      END
      SUBROUTINE REGRID (UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   Converts input spectral data to another number of channels
C   Inputs:
C      UVIN    C*?   Name of input uvdata object.
C      UVOUT   C*?   Name of output uvdata object. (may be UVIN)
C   Output:
C       IERR   I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IERR
C                                        MAXDIM = maximum number of axes
C                                                 in a visibility group
      INTEGER   MAXDIM
      PARAMETER (MAXDIM = 7)
C
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'REGRID.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CDUMMY, NAME*12, FCLAS*6, ONAME*12, OCLAS*6, UVTYPE*2,
     *   CSORT*2, TTYPE*2
      HOLLERITH LCATH(256)
      INTEGER   TYPE, DIM(3), COUNT, LREC, NRPARM, NCOR, ILOCU, ILOCV,
     *   ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1, ILOCA2, ILOCSA,
     *   JLOCC, JLOCS, JLOCF, JLOCIF, JLOCR, JLOCD, INCS, INCF, INCIF,
     *   NDIM, NAXIS(MAXDIM), DUMMY, FINDEX, IMSEQ, DISK, NUMFRQ,
     *   NUMPOL, NUMIF, IIF, ICHAN, IPOLN, INDEX, NCRFN, ANT1, ANT2, I,
     *   OLOCU, OLOCV, OLOCW, OLOCT, OLOCB, OLOCSU, OLOCFQ, KLOCC,
     *   KLOCS, KLOCF, KLOCIF, KLOCR, KLOCD, ONCS, ONCF, ONCIF,
     *   NAXISO(MAXDIM), OCOUNT, LCATI(256), OLOCA1, OLOCA2, OLOCSA
      REAL      RPS(50), VSS(3,MXVS), TDAT(2,2*MXVS), WORK(2,2*MXVS),
     *   AVWT, CDELT(MAXDIM), CDELTO(MAXDIM), R1, R2, CRPIX(MAXDIM),
     *   CRPIXO(MAXDIM), CHRATI, ALTRPX, AVCAL, INTPRM(3), VDAT(3,MXVS),
     *   VOUT(3,MXVS)
      LOGICAL   EXIST, ACDATA, DOACOR, ISMULT
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (LCATI, LCATH)
      EQUIVALENCE (VDAT, TDAT), (WORK, VOUT)
C-----------------------------------------------------------------------
      IERR = 0
C                                       Allow reading AC data
      DIM(1) = 1
      DIM(2) = 1
      DOACOR = .TRUE.
      CALL SECPUT (UVIN, 'DOACOR', OOALOG, DIM, DOACOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open input
      CALL OUVOPN (UVIN, 'RRAW', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBHGET (UVIN, LCATI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Create output if necessary
      CALL OBFEXS (UVOUT, EXIST, IERR)
      IF (IERR.GT.1) GO TO 990
      IERR = 0
      IF (.NOT.EXIST) THEN
         CALL OUVCRE (UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       interpolation type
      CALL OGET (UVIN, 'INTPARM', TYPE, DIM, INTPRM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get # freq channels + other
C                                       stuff
      CALL UVDGET (UVIN, 'NDIM', TYPE, DIM, NDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVIN, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDFND (UVIN, 2, 'FREQ', FINDEX, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVIN, 'CDELT', TYPE, DIM, CDELT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVIN, 'CRPIX', TYPE, DIM, CRPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL VELGET (UVIN, 'ALTRPIX', TYPE, DIM, ALTRPX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, CSORT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FNAGET (UVIN, 'NAME', TYPE, DIM, DUMMY, ONAME, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FNAGET (UVIN, 'CLASS', TYPE, DIM, DUMMY, OCLAS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FNAGET (UVIN, 'IMSEQ', TYPE, DIM, IMSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FNAGET (UVIN, 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C
      CALL UVDCOP (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDSCP (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Change size of output file
      CALL COPY (MAXDIM, NAXIS, NAXISO)
      NAXISO(FINDEX) = NCHAN
      NCRFN = NCHAN*2
C                                       Check we are doing something
      IF (NCHAN.EQ.NAXIS(FINDEX)) THEN
         IERR = 1
         WRITE (MSGTXT,1020) NCHAN
         GO TO 995
         END IF
C                                       Change freq increment
      CALL RCOPY (MAXDIM, CDELT, CDELTO)
      R1 = NAXIS(FINDEX)
      R2 = NCHAN
      CHRATI = R1/R2
      CDELTO(FINDEX) = CDELT(FINDEX) * (R1/R2)
C                                       Deal with reference pixel
      CALL RCOPY (MAXDIM, CRPIX, CRPIXO)
      CRPIXO(FINDEX) = 1.0 + ((CRPIX(FINDEX) - 1.0) *
     *   (CDELT(FINDEX) / CDELTO(FINDEX)))
      ALTRPX = 1.0 + ((ALTRPX - 1.0) * (CDELT(FINDEX) / CDELTO(FINDEX)))
C
      DIM(1) = 7
      DIM(2) = 1
      CALL UVDPUT (UVOUT, 'NAXIS', OOAINT, DIM, NAXISO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDPUT (UVOUT, 'CDELT', OOARE, DIM, CDELTO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDPUT (UVOUT, 'CRPIX', OOARE, DIM, CRPIXO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 2
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, CSORT, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      CALL VELPUT (UVOUT, 'ALTRPIX', OOARE, DIM, ALTRPX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get outname of file
      CALL INGET ('Input', 'OUTNAME', TYPE, DIM, DUMMY, NAME, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (NAME.EQ.' ') NAME = ONAME
      DIM(1) = 12
      CALL FNAPUT (UVOUT, 'NAME', OOACAR, DIM, DUMMY, NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get outclass of file
      CALL INGET ('Input', 'OUTCLASS', TYPE, DIM, DUMMY, FCLAS, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (FCLAS.EQ.' ') FCLAS = 'SPECR'
      DIM(1) = 6
      CALL FNAPUT (UVOUT, 'CLASS', OOACAR, DIM, DUMMY, FCLAS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get outseq of file
      CALL INGET ('Input', 'OUTSEQ', TYPE, DIM, IMSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      CALL FNAPUT (UVOUT, 'IMSEQ', OOAINT, DIM, IMSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get outdisk of file
      CALL INGET ('Input', 'OUTDISK', TYPE, DIM, DISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      CALL FNAPUT (UVOUT, 'DISK', OOAINT, DIM, DISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open output
      CALL OUVOPN (UVOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get number of visibilities
      CALL UVDGET (UVIN, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get info
C                                       Uv data pointers for input data
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Convert increments to work with
C                                       VS.
      INCS = INCS / 3
      INCF = INCF / 3
      INCIF = INCIF / 3
C                                       Axes dimensions
      NUMFRQ = NAXIS(JLOCF)
      NUMPOL = 1
      IF (JLOCS.GT.0) NUMPOL = NAXIS(JLOCS)
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = NAXIS(JLOCIF)
C                                       Uv data pointers for output data
      CALL UVDPNT (UVOUT, OLOCU, OLOCV, OLOCW, OLOCT, OLOCB, OLOCSU,
     *   OLOCFQ, OLOCA1, OLOCA2, OLOCSA, KLOCC, KLOCS, KLOCF, KLOCR,
     *   KLOCD, KLOCIF, ONCS, ONCF, ONCIF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Convert increments to work with
C                                       VSS
      CALL UVDGET (UVOUT, 'NAXIS', TYPE, DIM, NAXISO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      ONCS = ONCS / 3
      ONCF = ONCF / 3
      ONCIF = ONCIF / 3
C                                       LREC
      CALL UVDGET (UVIN, 'LREC', TYPE, DIM, LREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NRPARM
      CALL UVDGET (UVIN, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NCORR
      CALL UVDGET (UVIN, 'NCORR', TYPE, DIM, NCOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       baseline index
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check that it fits
      IF (NCOR.GT.MXVS) THEN
         WRITE (MSGTXT,1000) NCOR, MXVS
         IERR = 5
         GO TO 995
         END IF
C                                       Loop through input data
      OCOUNT = 0
      DO 200 I = 1, COUNT
         CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       Crack baseline
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
         ELSE
            ANT1 = RP(ILOCA1) + 0.001
            ANT2 = RP(ILOCA2) + 0.001
            END IF
         ACDATA = ANT1.EQ.ANT2
C                                       Loop over polzns/IF's
         DO 100 IPOLN = 1,NUMPOL
            DO 90 IIF = 1,NUMIF
               AVWT = 0.0
               AVCAL = 0.0
C                                       Do each IF/polzn separately
C                                       classical interpolation
               IF (INTPRM(1).GE.1.0) THEN
                  DO 60 ICHAN = 1,NUMFRQ
                     INDEX = 1 + (IIF-1) * INCIF  + (IPOLN-1) * INCS
     *                  + (ICHAN-1) * INCF
                     VDAT(1,ICHAN) = VS(1,INDEX)
                     IF ((ACDATA) .AND. (IPOLN.LE.2)) THEN
                        AVCAL = AVCAL + VS(2,INDEX)
                        VDAT(2,ICHAN) = 0.0
                     ELSE
                        VDAT(2,ICHAN) = VS(2,INDEX)
                        END IF
                     VDAT(3,ICHAN) = VS(3,INDEX)
 60                  CONTINUE
                  AVCAL = AVCAL / NUMFRQ
C                                       interpolate to output
                  CALL INSPEC (NUMFRQ, NCHAN, INTPRM, VDAT, VOUT)
C                                       return output spectrum
                  DO 65 ICHAN = 1,NCHAN
                     INDEX = 1 + (IIF-1) * ONCIF  + (IPOLN-1) * ONCS
     *                  + (ICHAN-1) * ONCF
                     VSS(1,INDEX) = VOUT(1,ICHAN)
                     IF ((ACDATA) .AND. (IPOLN.LE.2)) THEN
                        VSS(2,INDEX) = AVCAL
                     ELSE
                        VSS(2,INDEX) = VOUT(2,ICHAN)
                        END IF
                     VSS(3,INDEX) = VOUT(3,ICHAN)
 65                  CONTINUE
C                                       FFT
               ELSE
                  DO 80 ICHAN = 1,NUMFRQ
                     INDEX = 1 + (IIF-1) * INCIF  + (IPOLN-1) * INCS
     *                  + (ICHAN-1) * INCF
                     TDAT(1,ICHAN) = VS(1,INDEX)
                     IF ((ACDATA) .AND. (IPOLN.LE.2)) THEN
                        AVCAL = AVCAL + VS(2,INDEX)
                        TDAT(2,ICHAN) = 0.0
                     ELSE
                        TDAT(2,ICHAN) = VS(2,INDEX)
                        END IF
                     AVWT = AVWT + VS(3,INDEX)
 80                  CONTINUE
                  AVWT = AVWT / NUMFRQ
                  AVCAL = AVCAL / NUMFRQ
C                                       FFT etc to output
                  CALL RESPEC (NUMFRQ, NCHAN, TDAT, WORK)
C                                       load output spectrum
                  DO 85 ICHAN = 1,NCHAN
                     INDEX = 1 + (IIF-1) * ONCIF  + (IPOLN-1) * ONCS
     *                  + (ICHAN-1) * ONCF
                     VSS(1,INDEX) = TDAT(1,ICHAN)
                     IF ((ACDATA) .AND. (IPOLN.LE.2)) THEN
                        VSS(2,INDEX) = AVCAL
                     ELSE
                        VSS(2,INDEX) = TDAT(2,ICHAN)
                        END IF
                     VSS(3,INDEX) = AVWT
 85                  CONTINUE
                  END IF
 90            CONTINUE
 100        CONTINUE
C
         OCOUNT = OCOUNT + 1
         CALL RCOPY (NRPARM, RP, RPS)
         CALL UVWRIT (UVOUT, RPS, VSS, IERR)
         IF (IERR.GT.0) GO TO 990
 200     CONTINUE
      DIM(1) = 2
      DIM(2) = 1
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, CSORT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close files
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       # o/p points
      WRITE (MSGTXT,1010) OCOUNT
      CALL MSGWRT (4)
C                                       Copy tables
       DO 300 I = 1,KIEXTN
         IF (LCATI(KIVER+I-1).GT.0) THEN
            CALL H2CHR (2, 1, LCATH(KHEXT+I-1), TTYPE)
            IF ((TTYPE.NE.'BP') .AND. (TTYPE.NE.'HI') .AND.
     *         (TTYPE.NE.'PL') .AND. (TTYPE.NE.'FQ')) THEN
               CALL UVTCOP (UVIN, UVOUT, TTYPE, 0, IERR)
C              IF (IERR.NE.0) GO TO 990
               END IF
            END IF
 300     CONTINUE
C                                       Change channel separation in FQ
C                                       table
      CALL FQUPDT (UVIN, UVOUT, CHRATI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       See if multi-source;if so, change
C                                       channel separation in SU table
      CALL MULSDB (LCATI, ISMULT)
      IF (ISMULT) CALL SUUPDT (UVOUT, CHRATI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Mark output as valid
      DIM(1) = 1
      CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'ERROR REGRIDING ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REGRID: TOO MANY CORRELATIONS ', I6,' > ', I6)
 1010 FORMAT ('REGRID: Wrote ',I7,' regridded visibilities')
 1020 FORMAT ('REGRID: # o/p chans ',I4,' same as input - ',
     *   'check NPOINTS')
      END
      SUBROUTINE RESPEC (NFIN, NFOUT, DATA, WORK)
C-----------------------------------------------------------------------
C   Routine to transform a complex spectrum DATA of length NFIN
C   to a complex correlation function, then pad/truncate, then transform
C   to a complex spectrum of length NFOUT.
C   Inputs:
C      NFIN    I      # frequency channels in input spectrum
C      NFOUT   I      # frequency channels in output spectrum
C   In/out:
C      DATA    C(*)   Complex spectrum
C      WORK    C(*)   Work array (same size as data array)
C-----------------------------------------------------------------------
      INTEGER   NFIN, NFOUT
      REAL      WORK(2,*), DATA(2,*)
C
      INTEGER   ISB, KSTART, KSTOP, K, FFTDIR, KPTS, KADD
C-----------------------------------------------------------------------
C                                       Set some parameters
      ISB = 1
      KPTS = 2 * NFIN
C                                       Fill lower sideband array
C                                       slots with zeroes
      KSTART = NFIN + 1
      KSTOP  = KPTS
      DO 10 K = KSTART,KSTOP
         DATA(1,K) = 0.0
         DATA(2,K) = 0.0
 10      CONTINUE
C                                       Transform to XCF
      FFTDIR = -ISB
      CALL FOURG (DATA, KPTS, FFTDIR, WORK)
C                                       Scale and pad
      IF (NFOUT.GE.NFIN) THEN
         DO 20 K = 1,NFIN
            DATA(1,K) = DATA(1,K) / KPTS
            DATA(2,K) = DATA(2,K) / KPTS
 20         CONTINUE
C                                       shift & scale
         KADD = (NFOUT - NFIN) * 2
         DO 25 K = KSTOP,KSTART,-1
            DATA(1,K+KADD) = DATA(1,K) / KPTS
            DATA(2,K+KADD) = DATA(2,K) / KPTS
 25         CONTINUE
C                                       0-fill center
         KADD = KADD * 2
         CALL RFILL (KADD, 0.0, DATA(1,KSTART))
C                                       Scale and truncate
      ELSE
         DO 30 K = 1,NFOUT
            DATA(1,K) = DATA(1,K) / KPTS
            DATA(2,K) = DATA(2,K) / KPTS
 30         CONTINUE
         KADD = (NFIN - NFOUT) * 2
         KSTART = NFOUT + 1
         KSTOP = 2 * NFOUT
         DO 35 K = KSTART,KSTOP
            DATA(1,K) = DATA(1,K+KADD) / KPTS
            DATA(2,K) = DATA(2,K+KADD) / KPTS
 35         CONTINUE
         END IF
C                                       FFT back
      FFTDIR = ISB
      KPTS = 2 * NFOUT
      CALL FOURG (DATA, KPTS, FFTDIR, WORK)
C
 999  RETURN
      END
      SUBROUTINE INSPEC (NIN, NOUT, INTPRM, VDAT, VOUT)
C-----------------------------------------------------------------------
C   Interpolates NIN points from VDAT to NOUIT points of VOUT under
C   control of INTPRM
C   Inputs:
C      NIN      I        Number points in input
C      NOUT     I        Number points in output
C      INTPRM   R(3)     Type, width, support of function
C      VDAT     R(3,*)   Input data
C   Outputs:
C      VOUT     R(3,*)   Output data - not same as VDAT
C-----------------------------------------------------------------------
      INTEGER   NIN, NOUT
      REAL      INTPRM(3), VDAT(3,*), VOUT(3,*)
C
      INTEGER   INTYPE, J1, J2, I, J, K, K1, K2
      REAL      HWIDTH, FWIDTH, DX, WT, SR, SI, SW, X, MWR(51), MWI(51),
     *   MEDIAN
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      DX = NIN
      DX = DX / NOUT
      INTYPE = INTPRM(1) + 0.01
      HWIDTH = INTPRM(3) / 2.0
      FWIDTH = INTPRM(2) / 2.0
      DX = FLOAT (NIN) / FLOAT (NOUT)
      J2 = 1
C                                       Hanning (linear)
      IF (INTYPE.EQ.1) THEN
         HWIDTH = MAX (HWIDTH, FWIDTH)
         DO 20 I = 1,NOUT
            X = (I - 1.0) * DX + 1.0
            J1 = J2
            J2 = NIN + 1
            SR = 0.0
            SI = 0.0
            SW = 0.0
            DO 10 J = J1,NIN
               IF (ABS(X-J).LE.HWIDTH) THEN
                  J2 = MIN (J2, J)
                  IF ((ABS(X-J).LE.FWIDTH) .AND. (VDAT(3,J).GT.0.0))
     *               THEN
                     WT = ABS (X-J) / FWIDTH
                     WT = MAX (0.0, 1.0 - WT)
                     WT = WT * VDAT(3,J)
                     SR = SR + WT * VDAT(1,J)
                     SI = SI + WT * VDAT(2,J)
                     SW = SW + WT
                     END IF
               ELSE
                  IF (J.GT.X) GO TO 15
                  END IF
 10            CONTINUE
 15         IF (SW.GT.0.0) THEN
               VOUT(1,I) = SR / SW
               VOUT(2,I) = SI / SW
               VOUT(3,I) = SW
            ELSE
               VOUT(1,I) = 0.0
               VOUT(2,I) = 0.0
               VOUT(3,I) = 0.0
               END IF
 20         CONTINUE
C                                       Gaussian
      ELSE IF (INTYPE.EQ.2) THEN
         FWIDTH = FWIDTH / SQRT (LOG (2.0))
         DO 40 I = 1,NOUT
            X = (I - 1.0) * DX + 1.0
            J1 = J2
            J2 = NIN + 1
            SR = 0.0
            SI = 0.0
            SW = 0.0
            DO 30 J = J1,NIN
               IF (ABS(X-J).LE.HWIDTH) THEN
                  J2 = MIN (J2, J)
                  IF (VDAT(3,J).GT.0.0) THEN
                     WT = ABS (X-J) / FWIDTH
                     WT = EXP (-WT * WT)
                     WT = WT * VDAT(3,J)
                     SR = SR + WT * VDAT(1,J)
                     SI = SI + WT * VDAT(2,J)
                     SW = SW + WT
                     END IF
               ELSE
                  IF (J.GT.X) GO TO 35
                  END IF
 30            CONTINUE
 35         IF (SW.GT.0.0) THEN
               VOUT(1,I) = SR / SW
               VOUT(2,I) = SI / SW
               VOUT(3,I) = SW
            ELSE
               VOUT(1,I) = 0.0
               VOUT(2,I) = 0.0
               VOUT(3,I) = 0.0
               END IF
 40         CONTINUE
C                                       Boxcar
      ELSE IF (INTYPE.EQ.3) THEN
         HWIDTH = MAX (HWIDTH, FWIDTH)
         DO 60 I = 1,NOUT
            X = (I - 1.0) * DX + 1.0
            J1 = J2
            J2 = NIN + 1
            SR = 0.0
            SI = 0.0
            SW = 0.0
            DO 50 J = J1,NIN
               IF (ABS(X-J).LE.HWIDTH) THEN
                  J2 = MIN (J2, J)
                  IF ((ABS(X-J).LE.FWIDTH) .AND. (VDAT(3,J).GT.0.0))
     *               THEN
                     WT = VDAT(3,J)
                     SR = SR + WT * VDAT(1,J)
                     SI = SI + WT * VDAT(2,J)
                     SW = SW + WT
                     END IF
               ELSE
                  IF (J.GT.X) GO TO 55
                  END IF
 50            CONTINUE
 55         IF (SW.GT.0.0) THEN
               VOUT(1,I) = SR / SW
               VOUT(2,I) = SI / SW
               VOUT(3,I) = SW
            ELSE
               VOUT(1,I) = 0.0
               VOUT(2,I) = 0.0
               VOUT(3,I) = 0.0
               END IF
 60         CONTINUE
C                                       Sinc
      ELSE IF (INTYPE.EQ.4) THEN
         FWIDTH = FWIDTH / PI
         DO 80 I = 1,NOUT
            X = (I - 1.0) * DX + 1.0
            J1 = J2
            J2 = NIN + 1
            SR = 0.0
            SI = 0.0
            SW = 0.0
            DO 70 J = J1,NIN
               IF (ABS(X-J).LE.HWIDTH) THEN
                  J2 = MIN (J2, J)
                  IF (VDAT(3,J).GT.0.0) THEN
                     WT = ABS (X-J) / FWIDTH
                     IF (WT.LE.0.0) THEN
                        WT = 1.0
                     ELSE
                        WT = SIN (WT) / WT
                        END IF
                     WT = WT * VDAT(3,J)
                     SR = SR + WT * VDAT(1,J)
                     SI = SI + WT * VDAT(2,J)
                     SW = SW + WT
                     END IF
               ELSE
                  IF (J.GT.X) GO TO 75
                  END IF
 70            CONTINUE
 75         IF (SW.NE.0.0) THEN
               VOUT(1,I) = SR / SW
               VOUT(2,I) = SI / SW
               VOUT(3,I) = ABS (SW)
            ELSE
               VOUT(1,I) = 0.0
               VOUT(2,I) = 0.0
               VOUT(3,I) = 0.0
               END IF
 80         CONTINUE
C                                       Exponential
      ELSE IF (INTYPE.EQ.5) THEN
         FWIDTH = FWIDTH / LOG (2.0)
         DO 100 I = 1,NOUT
            X = (I - 1.0) * DX + 1.0
            J1 = J2
            J2 = NIN + 1
            SR = 0.0
            SI = 0.0
            SW = 0.0
            DO 90 J = J1,NIN
               IF (ABS(X-J).LE.HWIDTH) THEN
                  J2 = MIN (J2, J)
                  IF (VDAT(3,J).GT.0.0) THEN
                     WT = ABS (X-J) / FWIDTH
                     WT = EXP (-WT)
                     WT = WT * VDAT(3,J)
                     SR = SR + WT * VDAT(1,J)
                     SI = SI + WT * VDAT(2,J)
                     SW = SW + WT
                     END IF
               ELSE
                  IF (J.GT.X) GO TO 95
                  END IF
 90            CONTINUE
 95         IF (SW.GT.0.0) THEN
               VOUT(1,I) = SR / SW
               VOUT(2,I) = SI / SW
               VOUT(3,I) = SW
            ELSE
               VOUT(1,I) = 0.0
               VOUT(2,I) = 0.0
               VOUT(3,I) = 0.0
               END IF
 100        CONTINUE
C                                       MWF
      ELSE IF (INTYPE.EQ.6) THEN
         HWIDTH = MAX (HWIDTH, FWIDTH)
         FWIDTH = MIN (FWIDTH, 25.0)
         DO 120 I = 1,NOUT
            X = (I - 1.0) * DX + 1.0
            J1 = J2
            J2 = NIN + 1
            K = 0
            DO 110 J = J1,NIN
               IF (ABS(X-J).LE.HWIDTH) THEN
                  J2 = MIN (J2, J)
                  IF ((ABS(X-J).LE.FWIDTH) .AND. (VDAT(3,J).GT.0.0))
     *               THEN
                     WT = VDAT(3,J)
                     K = K + 1
                     MWR(K) = VDAT(1,J)
                     MWI(K) = VDAT(2,J)
                     SW = SW + WT
                     END IF
               ELSE
                  IF (J.GT.X) GO TO 115
                  END IF
 110           CONTINUE
 115        IF (K.GT.0) THEN
               VOUT(1,I) = MEDIAN (K, MWR)
               VOUT(2,I) = MEDIAN (K, MWI)
               VOUT(3,I) = SW / K
            ELSE
               VOUT(1,I) = 0.0
               VOUT(2,I) = 0.0
               VOUT(3,I) = 0.0
               END IF
 120        CONTINUE
C                                       2 PT
      ELSE IF (INTYPE.EQ.7) THEN
         DO 140 I = 1,NOUT
            X = (I - 1.0) * DX + 1.0
            J1 = J2
            J2 = NIN + 1
            K1 = 0
            K2 = NIN + 1
C                                       find low
            DO 130 J = J1,NIN
               IF (VDAT(3,J).GT.0.0) THEN
                  IF (X.GE.J) THEN
                     K1 = MAX (K1, J)
                  ELSE
                     K2 = MIN (K2, J)
                     GO TO 135
                     END IF
                  END IF
 130           CONTINUE
 135        WT = -1.0
            IF (K2.LE.NIN) THEN
               IF (K1.GT.0) THEN
                  WT = (X - K1) / FLOAT (K2 - K1)
               ELSE
                  WT = 1.0
                  END IF
            ELSE IF (K1.GT.0) THEN
               WT = 0.0
               END IF
            J2 = MAX (1, K1)
            IF (WT.GE.0.0) THEN
               VOUT(1,I) = (1.-WT) * VDAT(1,K1) + WT * VDAT(1,K2)
               VOUT(2,I) = (1.-WT) * VDAT(2,K1) + WT * VDAT(2,K2)
               VOUT(3,I) = (1.-WT) * VDAT(3,K1) + WT * VDAT(3,K2)
            ELSE
               VOUT(1,I) = 0.0
               VOUT(2,I) = 0.0
               VOUT(3,I) = 0.0
               END IF
 140        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE FQUPDT (UVIN, UVOUT, CHNRAT, IERR)
C-----------------------------------------------------------------------
C   Update the FQ table with the new channel width.
C   Inputs:
C      UVOUT   C*?  Name of output uvdata object.
C      CHNRAT  R    channel width ratio
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER UVIN*(*), UVOUT*(*)
      REAL      CHNRAT
      INTEGER   IERR
C
      INTEGER   FQID, FQROW, NUMIF, I, J, CFQROW, IFSIDE(MAXIF), TYPE,
     *   DIM(3)
      DOUBLE PRECISION IFFREQ(MAXIF)
      REAL      IFCHW(MAXIF), IFTBW(MAXIF)
      CHARACTER TABI*32, TABO*32, BNDCOD(MAXIF)*8, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       get FQ object
      TABI = 'FQUPDT temp. table in'
      CALL UV2TAB (UVIN, TABI, 'FQ', 1, IERR)
      IF (IERR.NE.0) GO TO 990
      TABO = 'FQUPDT temp. table out'
      CALL UV2TAB (UVOUT, TABO, 'FQ', 1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       open the table
      CALL OFQINI (TABI, 'READ', FQROW, NUMIF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OFQINI (TABO, 'WRIT', I, NUMIF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABGET (TABI, 'NROW', TYPE, DIM, FQROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Read and update
      DO 100 I = 1,FQROW
         CFQROW = I
         CALL OTABFQ (TABI, 'READ', CFQROW, NUMIF, FQID, IFFREQ, IFCHW,
     *      IFTBW, IFSIDE, BNDCOD, IERR)
         IF (IERR.NE.0) GO TO 990
         DO 50 J = 1,NUMIF
            IFCHW(J) = IFCHW(J) * CHNRAT
 50         CONTINUE
         CFQROW = I
         CALL OTABFQ (TABO, 'WRIT', CFQROW, NUMIF, FQID, IFFREQ, IFCHW,
     *      IFTBW, IFSIDE, BNDCOD, IERR)
         IF (IERR.NE.0) GO TO 990
 100     CONTINUE
C                                       close
      CALL OTABFQ (TABI, 'CLOS', CFQROW, NUMIF, FQID, IFFREQ, IFCHW,
     *   IFTBW, IFSIDE, BNDCOD, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OTABFQ (TABO, 'CLOS', CFQROW, NUMIF, FQID, IFFREQ, IFCHW,
     *   IFTBW, IFSIDE, BNDCOD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Destroy temp. table objects
      CALL TABDES (TABI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABDES (TABO, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'FQUPDT: ERROR UPDATING FQ TABLE IN ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE SUUPDT (UVOUT, CHNRAT, IERR)
C-----------------------------------------------------------------------
C   Update the SU table with the new channel width.
C   Inputs:
C      UVOUT   C*?  Name of output uvdata object.
C      CHNRAT  R    channel width ratio
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER UVOUT*(*)
      REAL      CHNRAT
      INTEGER   IERR
C
      INTEGER   SUID, SUROW, NUMIF, I, CSUROW, QUAL, FQID
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH,
     *   RAAPP, DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC,
     *   RAOBS, DECOBS
      REAL      FLUX(4,MAXIF)
      CHARACTER TAB*32, VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       get SU object
      TAB = 'SUUPTD temp. table 1'
      CALL UV2TAB (UVOUT, TAB, 'SU', 1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open the table read to get the
C                                       keyword values.
      CALL OSUINI (TAB, 'READ', NUMIF, VELTYP, VELDEF, FQID, SUROW,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OTABSU (TAB, 'CLOS', CSUROW, SUID, SOUNAM, QUAL,
     *   CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IERR)
C                                       open the table
      CALL OSUINI (TAB, 'WRIT', NUMIF, VELTYP, VELDEF, FQID, SUROW,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Read and update
      SUROW = SUROW - 1
      DO 100 I = 1, SUROW
         CSUROW = I
         CALL OTABSU (TAB, 'READ', CSUROW, SUID, SOUNAM, QUAL,
     *      CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *      DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IERR)
         IF (IERR.NE.0) GO TO 990
         BANDW = BANDW * CHNRAT
         CSUROW = I
         CALL OTABSU (TAB, 'WRIT', CSUROW, SUID, SOUNAM, QUAL,
     *      CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *      DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IERR)
         IF (IERR.NE.0) GO TO 990
 100     CONTINUE
C                                       close
      CALL OTABSU (TAB, 'CLOS', CSUROW, SUID, SOUNAM, QUAL, CALCOD,
     *   FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Destroy temp. table objects
      CALL TABDES (TAB,  IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'SUUPTD: ERROR UPDATING SU TABLE IN ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
