bit(b0).
bit(b1).

uint8(B0, B1, B2, B3, B4, B5, B6, B7) :-
	bit(B0), bit(B1), bit(B2), bit(B3),
	bit(B4), bit(B5), bit(B6), bit(B7).

uint16(Low8, High8) :- uint8(Low8), uint8(High8).
uint32(Low16, High16) :- uint16(Low16), uint16(High16).

bit_xor(b0, b0, b0).
bit_xor(b0, b1, b1).
bit_xor(b1, b0, b1).
bit_xor(b1, b1, b0).

bit_and(b0, b0, b0).
bit_and(b0, b1, b0).
bit_and(b1, b0, b0).
bit_and(b1, b1, b1).

bit_halfadd(B1, B2, Res, Carry) :-
	bit_xor(B1, B2, Res), 
	bit_and(B1, B2, Carry).

bit_fulladd(B1, B2, PrevC, Res, NextC) :-
	bit_halfadd(B1, B2, R, C0),
	bit_halfadd(R, PrevC, Res, C1),
	bit_xor(C0, C1, NextC).

uint8_inc(uint8(B0, B1, B2, B3, B4, B5, B6, B7),
					uint8(R0, R1, R2, R3, R4, R5, R6, R7),
					Carry) :-
	bit_halfadd(B0, b0, R0, C0),
	bit_halfadd(B1, C0, R1, C1),
	bit_halfadd(B2, C1, R2, C2),
	bit_halfadd(B3, C2, R3, C3),
	bit_halfadd(B4, C3, R4, C4),
	bit_halfadd(B5, C4, R5, C5),
	bit_halfadd(B6, C5, R6, C6),
	bit_halfadd(B7, C6, R7, Carry).

uint8_add(PrevC,
					uint8(A0, A1, A2, A3, A4, A5, A6, A7),
					uint8(B0, B1, B2, B3, B4, B5, B6, B7),
					uint8(C0, C1, C2, C3, C4, C5, C6, C7),
					Carry) :-
	bit_fulladd(A0, B0, PrevC,	C0, Carry0),
	bit_fulladd(A1, B1, Carry0, C1, Carry1),
	bit_fulladd(A2, B2, Carry1, C2, Carry2),
	bit_fulladd(A3, B3, Carry2, C3, Carry3),
	bit_fulladd(A4, B4, Carry3, C4, Carry4),
	bit_fulladd(A5, B5, Carry4, C5, Carry5),
	bit_fulladd(A6, B6, Carry5, C6, Carry6),
	bit_fulladd(A7, B7, Carry6, C7, Carry).
	
uint16_add(PrevC, uint16(U8_1L, U8_1H), uint16(U8_2L, U8_2H), uint16(U8_RL, U8_RH), C) :-
	uint8_add(PrevC, U8_1L, U8_2L, U8_RL, C0),
	uint8_add(C0, 	 U8_1H, U8_2H, U8_RH, C).

uint32_add(PrevC, uint32(U16_1L, U16_1H), uint32(U16_2L, U16_2H), C) :-
	uint16_add(PrevC, U16_1L, U16_2L, C0),
	uint16_add(C0,    U16_1H, U16_2H, C).

uint8_leq(uint8( _,  _,  _,  _,  _,  _,  _, b0), uint8( _,  _,  _,  _,  _,  _,  _, b1)).
uint8_leq(uint8( _,  _,  _,  _,  _,  _, b0, B7), uint8( _,  _,  _,  _,  _,  _, b1, B7)).
uint8_leq(uint8( _,  _,  _,  _,  _, b0, B6, B7), uint8( _,  _,  _,  _,  _, b1, B6, B7)).
uint8_leq(uint8( _,  _,  _,  _, b0, B5, B6, B7), uint8( _,  _,  _,  _, b1, B5, B6, B7)).
uint8_leq(uint8( _,  _,  _, b0, B4, B5, B6, B7), uint8( _,  _,  _, b1, B4, B5, B6, B7)).
uint8_leq(uint8( _,  _, b0, B3, B4, B5, B6, B7), uint8( _,  _, b1, B3, B4, B5, B6, B7)).
uint8_leq(uint8( _, b0, B2, B3, B4, B5, B6, B7), uint8( _, b1, B2, B3, B4, B5, B6, B7)).
uint8_leq(uint8(b0, B1, B2, B3, B4, B5, B6, B7), uint8(b1, B1, B2, B3, B4, B5, B6, B7)).
uint8_leq(uint8(B0, B1, B2, B3, B4, B5, B6, B7), uint8(B0, B1, B2, B3, B4, B5, B6, B7)).

uint8_eq(X, Y) :- uint8_leq(X, Y), uint8_leq(Y, X).

% usage:
% 	uint8_sh(NewMSB, 		 U8, 			U8 >> 1, DroppedLSB)
% 	uint8_sh(DroppedMSB, U8 << 1, U8, 	   NewLSB)
uint8_sh(C, uint8(B0, B1, B2, B3, B4, B5, B6, B7), uint8(B1, B2, B3, B4, B5, B6, B7, C), B0).

uint16_leq(uint16(U8_1L, U8_1H), uint16(U8_2L, U8_2H)) :-	
	U8_1H = U8_2H,
	uint8_leq(U8_1L, U8_2L).
uint16_leq(uint16(_, U8_1H), uint16(_, U8_2H)) :-	
	uint8_leq(U8_1H, U8_2H).

%% 

uint_lst(1, [], 0).
uint_lst(Div, [b0|BI], Res) :-
	Div1 is Div div 2, uint_lst(Div1, BI, Res).
uint_lst(Div, [b1|BI], Res) :-
	Div1 is Div div 2, uint_lst(Div1, BI, Res1), Res is Res1 + Div1.

uint8_num(uint8(B0, B1, B2, B3, B4, B5, B6, B7), I) :-
	uint_lst(256, [B7, B6, B5, B4, B3, B2, B1, B0], I).
