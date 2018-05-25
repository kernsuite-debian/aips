      SUBROUTINE TABHF (OPCODE, BUFFER, IHFRNO, HFKOLS, HFNUMV, 
     *   CC1, IC2, IC3, IC4, IC5, IC6, IC7, IC8, IC9, IC10, IC11, IC12,
     *   IC13, IC14, IC15, IC16, IC17, IC18, IC19, IC20, IC21, IC22,
     *   IC23, IC24, CC25, CC26, CC27, CC28, CC29, CC30, CC31, CC32,
     *   CC33, CC34, CC35, CC36, CC37, CC38, DC39, DC40, DC41, DC42,
     *   DC43, DC44, DC45, DC46, DC47, DC48, DC49, DC50, RC51, RC52,
     *   RC53, RC54, RC55, RC56, RC57, RC58, RC59, RC60, RC61, RC62,
     *   RC63, RC64, RC65, RC66, RC67, RC68, RC69, RC70, RC71, RC72,
     *   RC73, RC74, RC75, RC76, RC77,
     *   IERR)
C-----------------------------------------------------------------------
C! Does I/O to Haystack MkIII FRNGE tables (HF)
C# EXT-util VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C   Does I/O to MkIII FRNGE (VLBI fringe fitting) tables. Usually used
C   after setup by HFINI.
C    Inputs:
C     OPCODE   C*4     Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     BUFFER   I(1024) I/O buffer and related storage, also defines file
C                      if open. Should have been returned by TABINI or
C                      TABINI.
C     IHFRNO   I       Next scan number to read or write.
C     HFKOLS   I(*)    The column pointer array in order
C     HFNUMV   I(*)    Element count in each column.
C    Input/output: (written to or read from HF file)
C       CC1   C*2   Baseline code
C       IC2   I(6)  UTC time tag (YMDHMS) EPOCH for delay and rate
C       IC3   I(6)  UTC of FRNGE processing (YMDHMS)
C       IC4   I(4)  ARCHIV    
C       IC5   I     Sample rate in kbits/sec
C       IC6   I     no. frames per parameter period
C       IC7   I     Pass number
C       IC8   I     #U/L sideband pairs included in this processing
C                   (i.e. # of discrete LO frequencies)
C       IC9   I(28) #Accumulation periods by sideband by channel
C       IC10  I(56) (2,2,14)  Track # table in order of:
C                      Station #
C                      Refer/partn Sideband
C                      Channel #
C       IC11  I     COREL version #
C       IC12  I(6)  UTC time tag (YMDHMS)  EPOCH for "central" EPOCH
C       IC13  I     LU of printout (for FRNGP)
C       IC14  I     Ref. tape drive number
C       IC15  I     Rem. tape drive number
C       IC16  I     Special options NAME(211)
C       IC17  I     Integer value of PARAM(250) [=999 for special PCAL
C                   by AP]
C       IC18  I(28) CORELXTNT(2,14)  COREL extent# from which each track
C                   is taken.
C       IC19  I(84) CALBYFRQ(3,2,14) Phase-cal amp, phase and freq by
C                   station and channel normalized as follows:
C                      AMP 0 to 10000(in voltage) -1=manual cal
C                      PHS -18000 to +18000
C                      frq  kHz      
C       IC20  I(28) PROCUTC(2,14) COREL processing UT (YDDD) by sideband
C                   and channel.
C       IC21  I(56) ERRORATE(2,2,14) Tape error rate by station,
C                   sideband and channel encoded as:
C                      1000*LOGT(error rate)
C                      (=-32000 if no errors)
C       IC22  I(28) INDEX(2,14) COREL index #s by sideband and FRNGE CH#
C       IC23  I     FRNGE error code 0=OK NE.0 Don't use
C       IC24  I     SBDOFFST flag 1=this run had sideband fixup
C       CC25  C*8   STAR ID, Radio source name
C       CC26  C*8   First antenna name of baseline
C       CC27  C*8   Second antenna of baseline
C       CC28  C*6   CORELFILE COREL correlation output file name
C       CC29  C*8   first raw-data tape ID label
C       CC30  C*8   second raw-data tape ID label
C       CC31  C*6   VLB2PRG FRNGE program version YYMMDD
C       CC32  C*8   RUN CODE  Run code, e.g. "329-1300"
C       CC33  C*1   FRNGE quality code 0=no good, 1=very poor, 9=very
C                   good, A=has FRNGE error code=1,B=2 etc.
C       CC34  C*2   Frequency group code
C       CC35  C*6   Original COREL file name
C       CC36  C*6   Tape Q code
C       CC37  C*8   Ref station occupation code
C       CC38  C*8   Rem station occupation code
C       DC39  D(14) RFREQ(14) LO frequencies (MHz) by channel
C       DC40  D     REF FREQ RF freq (MHz) to which phase is referred 
C       DC41  D     DEL OBSV Observed group delay in microseconds
C                   equals single band delay if only one freq.
C                   processed.  DELOBSV=T2=T1, T1=time of arrival at
C                   site 1 (reference site) as measured by the site
C                   clock at site 1, T2=time of signal arrival at site 2
C                   as measured by site clock at site 2.  The signal in
C                   question is the one which arrives at site 1 at a UTC
C                   time equal to that given as the UTC epoch.
C       DC42  D     RAT OBSV  Observed delay rate (usec/sec) corrected
C                   for the phase cal rate.
C       DC43  D     NB DELAY Narrow-band group-delay (usec)
C       DC44  D     DGPD group delay ambig. (usec)
C       DC45  D     BTE0 Apriori clock (usec)
C       DC46  D     EPOCH0  ref. st. clock epoch (usec)
C       DC47  D     DEL OBSVM observed delay at central epoch
C       DC48  D     RAT OBSVM observed delay rate at central epoch
C       DC49  D     DLY2 phase delay at EPO+1 sec
C       DC50  D     DLY3 phase delay at EPO-1 sec.
C       RC51  R(28) AMBYFRQ(2,14) Amp and phase by channel 1=100%
C                   phase=-180 to 180 deg. residual to COREL and
C                   uncorrected for PCAL rate.
C       RC52  R(2)  Phase cal rate by station (usec/sec)
C       RC53  R     DELRESID Delay residual to COREL a priori.
C       RC54  R     DELSIGMA Calculated delay error (usec)
C       RC55  R     RATRESID Delay-rate residual to COREL a priori
C                   (usec/sec) corrected for phase cal rates.
C       RC56  R     RATSIGMA Calculate delay-rate error (usec/sec)
C       RC57  R     COHERCOR Coherent multi-freq correlation
C                   coefficient. (1=100% correlation)
C       RC58  R     TOTPHASE Total observed fringe phase (deg)
C       RC59  R(2)  UVF/ASEC Fringes per asec in N-S and E-W
C       RC60  R(2)  STARELEV Calculated star elevations run reference
C                   time by station (deg).
C       RC61  R     AAMP FRNGE amplitude fro incoherent addition of
C                   frequency channels.
C       RC62  R(2)  URVRSEC Rate derivatives mHz/sec arc
C       RC63  R(6)  SRCHPAR Search parameters
C       RC64  R     DEPSBRES Single band delay residual (usec)
C       RC65  R     SNR Signal to noise ratio in sigma.
C       RC66  R     PROB Probability of a false detection.
C       RC67  R     INCOH Incoherent segmented fringe amp. in units of
C                   10,000. 
C       RC68  R     EARP total phase refered to an epoch at time the
C                   signal reaches the center of teh earth.
C       RC69  R     REARP Residual phase corrected to earth centered
C                   epoch 
C       RC70  R     START Start time in seconds past hour
C       RC71  R     STOP Stop time in seconds past hour
C       RC72  R     EPD Epoch offset from center of run in sec.
C       RC73  R     DUR Effective run duration in seconds.
C       RC74  R     DELSS Single-band delay error in microsec.
C       RC75  R     QB Ratio of min to max data accepted in %
C       RC76  R     DISCD % data discarded
C       RC77  R     TOTPM Total phase at central epoch
C    Output:
C       IHFRNO I      Next row number.
C       IERR   I      Error code, 0=>OK else TABIO error.
C                    Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, 
     *   CC1*2, CC25*8, CC26*8, CC27*8, CC28*6, CC29*8, CC30*8, CC31*6,
     *   CC32*8, CC33*1, CC34*2, CC35*6, CC36*6, CC37*8, CC38*8
      INTEGER   BUFFER(*), IHFRNO, HFKOLS(*), HFNUMV(*), IERR,
     *   IC2(6), IC3(6), IC4(4), IC5, IC6, IC7, IC8, IC9(28), IC10(56),
     *   IC11, IC12(6), IC13, IC14, IC15, IC16, IC17, IC18(28),
     *   IC19(84), IC20(28), IC21(56), IC22(28), IC23, IC24
      REAL      RC51(28), RC52(2), RC53, RC54, RC55, RC56, RC57, RC58,
     *   RC59(2), RC60(2), RC61, RC62(2), RC63(6), RC64, RC65, RC66,
     *   RC67, RC68, RC69, RC70, RC71, RC72, RC73, RC74, RC75, RC76,
     *   RC77
      DOUBLE PRECISION DC39(14), DC40, DC41, DC42, DC43, DC44, DC45,
     *   DC46, DC47, DC48, DC49, DC50
C
      INCLUDE 'INCS:PUVD.INC'
C                                       MAXHFC = no. col. in table
      INTEGER   MAXHFC
      PARAMETER (MAXHFC = 77)
      INTEGER   RECI(512), NCOPY
      LOGICAL   DOREAD, DOCLOS
      REAL      RECORD(512)
      HOLLERITH RECH(512)
      DOUBLE PRECISION RECD(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RECD, RECORD, RECI, RECH)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
         CALL CHR2H (2, CC1, 1, RECH(HFKOLS(1)))
         CALL COPY (HFNUMV(2), IC2, RECI(HFKOLS(2)))
         CALL COPY (HFNUMV(3), IC3, RECI(HFKOLS(3)))
         CALL COPY (HFNUMV(4), IC4, RECI(HFKOLS(4)))
         CALL COPY (HFNUMV(5), IC5, RECI(HFKOLS(5)))
         CALL COPY (HFNUMV(6), IC6, RECI(HFKOLS(6)))
         CALL COPY (HFNUMV(7), IC7, RECI(HFKOLS(7)))
         CALL COPY (HFNUMV(8), IC8, RECI(HFKOLS(8)))
         CALL COPY (HFNUMV(9), IC9, RECI(HFKOLS(9)))
         CALL COPY (HFNUMV(10), IC10, RECI(HFKOLS(10)))
         CALL COPY (HFNUMV(11), IC11, RECI(HFKOLS(11)))
         CALL COPY (HFNUMV(12), IC12, RECI(HFKOLS(12)))
         CALL COPY (HFNUMV(13), IC13, RECI(HFKOLS(13)))
         CALL COPY (HFNUMV(14), IC14, RECI(HFKOLS(14)))
         CALL COPY (HFNUMV(15), IC15, RECI(HFKOLS(15)))
         CALL COPY (HFNUMV(16), IC16, RECI(HFKOLS(16)))
         CALL COPY (HFNUMV(17), IC17, RECI(HFKOLS(17)))
         CALL COPY (HFNUMV(18), IC18, RECI(HFKOLS(18)))
         CALL COPY (HFNUMV(19), IC19, RECI(HFKOLS(19)))
         CALL COPY (HFNUMV(20), IC20, RECI(HFKOLS(20)))
         CALL COPY (HFNUMV(21), IC21, RECI(HFKOLS(21)))
         CALL COPY (HFNUMV(22), IC22, RECI(HFKOLS(22)))
         CALL COPY (HFNUMV(23), IC23, RECI(HFKOLS(23)))
         CALL COPY (HFNUMV(24), IC24, RECI(HFKOLS(24)))
         CALL CHR2H (HFNUMV(25), CC25, 1, RECH(HFKOLS(25)))
         CALL CHR2H (HFNUMV(26), CC26, 1, RECH(HFKOLS(26)))
         CALL CHR2H (HFNUMV(27), CC27, 1, RECH(HFKOLS(27)))
         CALL CHR2H (HFNUMV(28), CC28, 1, RECH(HFKOLS(28)))
         CALL CHR2H (HFNUMV(29), CC29, 1, RECH(HFKOLS(29)))
         CALL CHR2H (HFNUMV(30), CC30, 1, RECH(HFKOLS(30)))
         CALL CHR2H (HFNUMV(31), CC31, 1, RECH(HFKOLS(31)))
         CALL CHR2H (HFNUMV(32), CC32, 1, RECH(HFKOLS(32)))
         CALL CHR2H (HFNUMV(33), CC33, 1, RECH(HFKOLS(33)))
         CALL CHR2H (HFNUMV(34), CC34, 1, RECH(HFKOLS(34)))
         CALL CHR2H (HFNUMV(35), CC35, 1, RECH(HFKOLS(35)))
         CALL CHR2H (HFNUMV(36), CC36, 1, RECH(HFKOLS(36)))
         CALL CHR2H (HFNUMV(37), CC37, 1, RECH(HFKOLS(37)))
         CALL CHR2H (HFNUMV(38), CC38, 1, RECH(HFKOLS(38)))
         NCOPY = HFNUMV(39) * NWDPDP
         CALL RCOPY (NCOPY, DC39, RECD(HFKOLS(39)))
         NCOPY = HFNUMV(40) * NWDPDP
         CALL RCOPY (NCOPY, DC40, RECD(HFKOLS(40)))
         NCOPY = HFNUMV(41) * NWDPDP
         CALL RCOPY (NCOPY, DC41, RECD(HFKOLS(41)))
         NCOPY = HFNUMV(42) * NWDPDP
         CALL RCOPY (NCOPY, DC42, RECD(HFKOLS(42)))
         NCOPY = HFNUMV(43) * NWDPDP
         CALL RCOPY (NCOPY, DC43, RECD(HFKOLS(43)))
         NCOPY = HFNUMV(44) * NWDPDP
         CALL RCOPY (NCOPY, DC44, RECD(HFKOLS(44)))
         NCOPY = HFNUMV(45) * NWDPDP
         CALL RCOPY (NCOPY, DC45, RECD(HFKOLS(45)))
         NCOPY = HFNUMV(46) * NWDPDP
         CALL RCOPY (NCOPY, DC46, RECD(HFKOLS(46)))
         NCOPY = HFNUMV(47) * NWDPDP
         CALL RCOPY (NCOPY, DC47, RECD(HFKOLS(47)))
         NCOPY = HFNUMV(48) * NWDPDP
         CALL RCOPY (NCOPY, DC48, RECD(HFKOLS(48)))
         NCOPY = HFNUMV(49) * NWDPDP
         CALL RCOPY (NCOPY, DC49, RECD(HFKOLS(49)))
         NCOPY = HFNUMV(50) * NWDPDP
         CALL RCOPY (NCOPY, DC50, RECD(HFKOLS(50)))
         CALL RCOPY (HFNUMV(51), RC51, RECORD(HFKOLS(51)))
         CALL RCOPY (HFNUMV(52), RC52, RECORD(HFKOLS(52)))
         CALL RCOPY (HFNUMV(53), RC53, RECORD(HFKOLS(53)))
         CALL RCOPY (HFNUMV(54), RC54, RECORD(HFKOLS(54)))
         CALL RCOPY (HFNUMV(55), RC55, RECORD(HFKOLS(55)))
         CALL RCOPY (HFNUMV(56), RC56, RECORD(HFKOLS(56)))
         CALL RCOPY (HFNUMV(57), RC57, RECORD(HFKOLS(57)))
         CALL RCOPY (HFNUMV(58), RC58, RECORD(HFKOLS(58)))
         CALL RCOPY (HFNUMV(59), RC59, RECORD(HFKOLS(59)))
         CALL RCOPY (HFNUMV(60), RC60, RECORD(HFKOLS(60)))
         CALL RCOPY (HFNUMV(61), RC61, RECORD(HFKOLS(61)))
         CALL RCOPY (HFNUMV(62), RC62, RECORD(HFKOLS(62)))
         CALL RCOPY (HFNUMV(63), RC63, RECORD(HFKOLS(63)))
         CALL RCOPY (HFNUMV(64), RC64, RECORD(HFKOLS(64)))
         CALL RCOPY (HFNUMV(65), RC65, RECORD(HFKOLS(65)))
         CALL RCOPY (HFNUMV(66), RC66, RECORD(HFKOLS(66)))
         CALL RCOPY (HFNUMV(67), RC67, RECORD(HFKOLS(67)))
         CALL RCOPY (HFNUMV(68), RC68, RECORD(HFKOLS(68)))
         CALL RCOPY (HFNUMV(69), RC69, RECORD(HFKOLS(69)))
         CALL RCOPY (HFNUMV(70), RC70, RECORD(HFKOLS(70)))
         CALL RCOPY (HFNUMV(71), RC71, RECORD(HFKOLS(71)))
         CALL RCOPY (HFNUMV(72), RC72, RECORD(HFKOLS(72)))
         CALL RCOPY (HFNUMV(73), RC73, RECORD(HFKOLS(73)))
         CALL RCOPY (HFNUMV(74), RC74, RECORD(HFKOLS(74)))
         CALL RCOPY (HFNUMV(75), RC75, RECORD(HFKOLS(75)))
         CALL RCOPY (HFNUMV(76), RC76, RECORD(HFKOLS(76)))
         CALL RCOPY (HFNUMV(77), RC77, RECORD(HFKOLS(77)))
         END IF
C                                       Process record.
      CALL TABIO (OPCODE, 0, IHFRNO, RECORD, BUFFER, IERR)
      IHFRNO = IHFRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
         CALL H2CHR (2, 1, RECH(HFKOLS(1)), CC1)
         CALL COPY (HFNUMV(2), RECI(HFKOLS(2)), IC2)
         CALL COPY (HFNUMV(3), RECI(HFKOLS(3)), IC3)
         CALL COPY (HFNUMV(4), RECI(HFKOLS(4)), IC4)
         CALL COPY (HFNUMV(5), RECI(HFKOLS(5)), IC5)
         CALL COPY (HFNUMV(6), RECI(HFKOLS(6)), IC6)
         CALL COPY (HFNUMV(7), RECI(HFKOLS(7)), IC7)
         CALL COPY (HFNUMV(8), RECI(HFKOLS(8)), IC8)
         CALL COPY (HFNUMV(9), RECI(HFKOLS(9)), IC9)
         CALL COPY (HFNUMV(10), RECI(HFKOLS(10)), IC10)
         CALL COPY (HFNUMV(11), RECI(HFKOLS(11)), IC11)
         CALL COPY (HFNUMV(12), RECI(HFKOLS(12)), IC12)
         CALL COPY (HFNUMV(13), RECI(HFKOLS(13)), IC13)
         CALL COPY (HFNUMV(14), RECI(HFKOLS(14)), IC14)
         CALL COPY (HFNUMV(15), RECI(HFKOLS(15)), IC15)
         CALL COPY (HFNUMV(16), RECI(HFKOLS(16)), IC16)
         CALL COPY (HFNUMV(17), RECI(HFKOLS(17)), IC17)
         CALL COPY (HFNUMV(18), RECI(HFKOLS(18)), IC18)
         CALL COPY (HFNUMV(19), RECI(HFKOLS(19)), IC19)
         CALL COPY (HFNUMV(20), RECI(HFKOLS(20)), IC20)
         CALL COPY (HFNUMV(21), RECI(HFKOLS(21)), IC21)
         CALL COPY (HFNUMV(22), RECI(HFKOLS(22)), IC22)
         CALL COPY (HFNUMV(23), RECI(HFKOLS(23)), IC23)
         CALL COPY (HFNUMV(24), RECI(HFKOLS(24)), IC24)
         CALL H2CHR (HFNUMV(25), 1, RECH(HFKOLS(25)), CC25)
         CALL H2CHR (HFNUMV(26), 1, RECH(HFKOLS(26)), CC26)
         CALL H2CHR (HFNUMV(27), 1, RECH(HFKOLS(27)), CC27)
         CALL H2CHR (HFNUMV(28), 1, RECH(HFKOLS(28)), CC28)
         CALL H2CHR (HFNUMV(29), 1, RECH(HFKOLS(29)), CC29)
         CALL H2CHR (HFNUMV(30), 1, RECH(HFKOLS(30)), CC30)
         CALL H2CHR (HFNUMV(31), 1, RECH(HFKOLS(31)), CC31)
         CALL H2CHR (HFNUMV(32), 1, RECH(HFKOLS(32)), CC32)
         CALL H2CHR (HFNUMV(33), 1, RECH(HFKOLS(33)), CC33)
         CALL H2CHR (HFNUMV(34), 1, RECH(HFKOLS(34)), CC34)
         CALL H2CHR (HFNUMV(35), 1, RECH(HFKOLS(35)), CC35)
         CALL H2CHR (HFNUMV(36), 1, RECH(HFKOLS(36)), CC36)
         CALL H2CHR (HFNUMV(37), 1, RECH(HFKOLS(37)), CC37)
         CALL H2CHR (HFNUMV(38), 1, RECH(HFKOLS(38)), CC38)
         NCOPY = HFNUMV(39) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(39)), DC39)
         NCOPY = HFNUMV(40) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(40)), DC40)
         NCOPY = HFNUMV(41) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(41)), DC41)
         NCOPY = HFNUMV(42) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(42)), DC42)
         NCOPY = HFNUMV(43) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(43)), DC43)
         NCOPY = HFNUMV(44) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(44)), DC44)
         NCOPY = HFNUMV(45) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(45)), DC45)
         NCOPY = HFNUMV(46) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(46)), DC46)
         NCOPY = HFNUMV(47) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(47)), DC47)
         NCOPY = HFNUMV(48) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(48)), DC48)
         NCOPY = HFNUMV(49) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(49)), DC49)
         NCOPY = HFNUMV(50) * NWDPDP
         CALL RCOPY (NCOPY, RECD(HFKOLS(50)), DC50)
         CALL RCOPY (HFNUMV(51), RECORD(HFKOLS(51)), RC51)
         CALL RCOPY (HFNUMV(52), RECORD(HFKOLS(52)), RC52)
         CALL RCOPY (HFNUMV(53), RECORD(HFKOLS(53)), RC53)
         CALL RCOPY (HFNUMV(54), RECORD(HFKOLS(54)), RC54)
         CALL RCOPY (HFNUMV(55), RECORD(HFKOLS(55)), RC55)
         CALL RCOPY (HFNUMV(56), RECORD(HFKOLS(56)), RC56)
         CALL RCOPY (HFNUMV(57), RECORD(HFKOLS(57)), RC57)
         CALL RCOPY (HFNUMV(58), RECORD(HFKOLS(58)), RC58)
         CALL RCOPY (HFNUMV(59), RECORD(HFKOLS(59)), RC59)
         CALL RCOPY (HFNUMV(60), RECORD(HFKOLS(60)), RC60)
         CALL RCOPY (HFNUMV(61), RECORD(HFKOLS(61)), RC61)
         CALL RCOPY (HFNUMV(62), RECORD(HFKOLS(62)), RC62)
         CALL RCOPY (HFNUMV(63), RECORD(HFKOLS(63)), RC63)
         CALL RCOPY (HFNUMV(64), RECORD(HFKOLS(64)), RC64)
         CALL RCOPY (HFNUMV(65), RECORD(HFKOLS(65)), RC65)
         CALL RCOPY (HFNUMV(66), RECORD(HFKOLS(66)), RC66)
         CALL RCOPY (HFNUMV(67), RECORD(HFKOLS(67)), RC67)
         CALL RCOPY (HFNUMV(68), RECORD(HFKOLS(68)), RC68)
         CALL RCOPY (HFNUMV(69), RECORD(HFKOLS(69)), RC69)
         CALL RCOPY (HFNUMV(70), RECORD(HFKOLS(70)), RC70)
         CALL RCOPY (HFNUMV(71), RECORD(HFKOLS(71)), RC71)
         CALL RCOPY (HFNUMV(72), RECORD(HFKOLS(72)), RC72)
         CALL RCOPY (HFNUMV(73), RECORD(HFKOLS(73)), RC73)
         CALL RCOPY (HFNUMV(74), RECORD(HFKOLS(74)), RC74)
         CALL RCOPY (HFNUMV(75), RECORD(HFKOLS(75)), RC75)
         CALL RCOPY (HFNUMV(76), RECORD(HFKOLS(76)), RC76)
         CALL RCOPY (HFNUMV(77), RECORD(HFKOLS(77)), RC77)
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IHFRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DATA TYPE FOR COLUMN ',I5, ' = ',I4,' IS NOT SUPPORTED')
 1800 FORMAT ('TABHF: TABIO ERROR',I3)
      END
