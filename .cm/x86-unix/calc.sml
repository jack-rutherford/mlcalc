110.79  x86    
         �   �       *      W4   Y��9�JY�u�*cU V   s )#��3�G���hQ�B         ��(����Ӓ��{��]  �A�>�K�p&��;�   ��%N���U�ǎ���  �{�j������΋8  ��ܝ���	�~@��	  ����x��$ɫwSs��  �4+ݸB�F�[!��P;  ���%HfV� �+џ  �Qyi��NxP\B"#�  ���\bcdZ�<�*�����\bcdZ�<�*��               n               nY��9�JY�u�*c)#��3�G���hQ�B��(����Ӓ��{��]�A�>�K�p&��;���%N���U�ǎ�����ܝ���	�~@��	����x��$ɫwSs���4+ݸB�F�[!��P;���%HfV� �+џ�Qyi��NxP\B"#���P(!zN<���Hn.guid-(sources.cm):calc.sml-1709669938.242
  �    �"   J   =   <>	      <>   =	      <=   >	      >=   <	      <   >=	      >   <=	           	         	         	         	         	      
   	      calc.sml:50.40-50.50    	      calc.sml:73.18-73.27    	      calc.sml:84.18-84.27    	          
   	, Error:     line    L   
notLocated   	unboundId    referenced or type error!
   Unbound identifier    Unimplemented   +   :=   /   :=-   *   MEM:=   -   :=MEM
   $An error occurred while compiling!

   equ nullchar M[16]
   equ nl M[15]
   equ cr M[14]
   equ SP M[13]
   equ MEM M[12]
   equ PR9 M[9]
   equ PR8 M[8]
   equ PR7 M[7]
   equ PR6 M[6]
   equ PR5 M[5]
   equ PR4 M[4]
   equ PR3 M[3]
   equ PR2 M[2]
   equ PR1 M[1]
   equ PR0 M[0]
   (PC:=PR8			# return from whence you came
   )SP:=M[SP+1]		# restore the stack pointer
   writeStr(cr)
   Koutput:  writeInt(PR9)		# write the integer in function parameter register
   ###### output function ######
   Finput:  readInt(PR9)		# read an integer into function result register
   ###### input function ######
   )
halt

   	writeInt(   nullchar:=0
   	nl := 10
   	cr := 13
   	PR9 := 0
   	PR8 := 0
   	PR7 := 0
   	PR6 := 0
   	PR5 := 0
   	PR4 := 0
   	PR3 := 0
   	PR2 := 0
   	PR1 := 0
   	PR0 := 0
   SP:=100
   a.ewe   usage: sml @SMLload=calc
	   ;               	   �  Qh    �D$H� �D$;|$��   �t$P���T$T�͋qX�Q��   �G   �G�  �i�o�n�o�n�o�G  �.�o�)�o �n�o$�v�w(�l$P�o,�_0�G4�G�G8�2�_�o�t$H�T$L�T$T�D$���  ��@�d$H�P  ����X����D$;|$w�   ;|$w�   ���PP  �   �   �   �   �D$H   �D$L   �T$ �\$��	����T$붐�������D$;|$��  ���  �V�W�_���W��ڋՋl$���  ���  �l$���  �o�(�o�G  �l$��  �o�h�o�G  �l$��h  �o�h�o �G$  �l$���  �o(�h�o,�G0  �l$��  �o4�h�o8�G<  �l$��X  �o@�h�oD�GH  �l$���  �oL�h�oP�GT  �l$���  �oX�h �o\�G`�  �o�od�o�oh�o�ol�o(�op�o4�ot�o@�ox�oL�o|�@���   �oX���   �od�ǈ   �搐��������D$;|$�K  �Ë�X�  �o�K<�O�G�  �W�o�o�K8�O�S4�W�k0�o �K,�G$�  ��W(�k�o,�S�W0�k�o4�S�W8�(�o<�@�G@�S�WD�k�oH�C�GL�S �WP�k$�oT�C(�GX�W�W\�i�o`�A�Gd��Wh�k@�ol�CD�Gp�SH�Wt�kL�ox�CP�G|�ST���   �kX���   �C\���   �ǐ   �_��A�Q�΋l$��  ��������������D$;|$�7  ��΋Ջl$��d  �������\����D$;|$�  ��m �  �G   �M�O�G�  �U�
�O�o�_�� �o��O�݋ЋƋl$��x  �>�������������D$;|$��   ��Ջ^�k�]�k�
  ��_�X�_�X�_�X�_�X�_�X�_�X�_�X�_ �X �_$�X$�_(�X(�_,�X,�_0�X0�_4�X4�_8�X8�_<�X<�_@�X@�_D�@D�GH�IT�OL�wP��X�_��E �M�l$���  �����WL  ;|$wD�D$L�t$`�L$d�H�1�E�D$\�E�D$X�E�D$T�E�D$P�m �t$H�L$L�t$`�L$d�d$H�L  �;|$wD�D$L�t$`�L$d�H�1�E�D$\�E�D$X�E�D$T�E�D$P�m �t$H�L$L�t$`�L$d�d$H��K  �;|$wD�D$L�t$`�L$d�H�1�E�D$\�E�D$X�E�D$T�E�D$P�m �t$H�L$L�t$`�L$d�d$H�~K  �;|$wD�D$L�t$`�L$d�H�1�E�D$\�E�D$X�E�D$T�E�D$P�m �t$H�L$L�t$`�L$d�d$H�.K  �;|$wD�D$L�t$`�L$d�H�1�E�D$\�E�D$X�E�D$T�E�D$P�m �t$H�L$L�t$`�L$d�d$H��J  �;|$wD�D$L�t$`�L$d�H�1�E�D$\�E�D$X�E�D$T�E�D$P�m �t$H�L$L�t$`�L$d�d$H�J  �;|$wD�D$L�t$`�L$d�H�1�E�D$\�E�D$X�E�D$T�E�D$P�m �t$H�L$L�t$`�L$d�d$H�>J  ��D$H������D$;|$wy�D$L�T$p�T$P�T$`�T$T�T$d�T$X�T$h�T$\�T$l�  �w�_�P�2�_�t$H�T$L�T$p�t$`�t$P�D$d�D$T�t$h�t$X�D$l�D$\�D$���  ���d$H�DI  ��;|$wB�L$`�T$d�L$P�T$T�t$X�D$\��  �o�O�W�w�G�3�o�[�L$`�T$d�����H  �����$����D$;|$��  �Ë�m �] ��  �K�O�M�O�M�O�M�O�M�O�m�o�G�� �O��ً΋t$���  ���  �l$���	  �o�(�o�G  �l$��\
  �o�h�o�G  �l$���
  �o�h�o �G$  �l$��4  �o(�h�o,�G0  �l$���  �o4�h�o8�G<  �l$��  �o@�h�oD�GH  �l$��x  �oL�h�oP�GT  �l$���  �oX�h �o\�G`�  �o�od�o�oh�o�ol�o(�op�o4�ot�o@�ox�oL�o|�@���   �oX���   �od�ǈ   �損������D$;|$w~������  �P�W�_���W� �ڋ֋l$��  ������d����D$;|$w>�u �.�  �M�O�E�G�M�O�_���_�E �V�M�l$��\	  �S����0G  �;|$w`�D$L�t$`�\$d�L$h�T$l�L$P�T$T�\$X�t$\��  �o�O�W�_�w�@��o�L$H�D$L�t$`�\$d�L$h�T$l���d$H�sF  �;|$w`�D$L�t$`�\$d�L$h�T$l�L$P�T$T�\$X�t$\��  �o�O�W�_�w�@��o�L$H�D$L�t$`�\$d�L$h�T$l���d$H�F  �;|$w`�D$L�t$`�\$d�L$h�T$l�L$P�T$T�\$X�t$\��  �o�O�W�_�w�@��o�L$H�D$L�t$`�\$d�L$h�T$l���d$H�E  �;|$w`�D$L�t$`�\$d�L$h�T$l�L$P�T$T�\$X�t$\��  �o�O�W�_�w�@��o�L$H�D$L�t$`�\$d�L$h�T$l���d$H�/E  �;|$w`�D$L�t$`�\$d�L$h�T$l�L$P�T$T�\$X�t$\��  �o�O�W�_�w�@��o�L$H�D$L�t$`�\$d�L$h�T$l���d$H��D  �;|$w`�D$L�t$`�\$d�L$h�T$l�L$P�T$T�\$X�t$\��  �o�O�W�_�w�@��o�L$H�D$L�t$`�\$d�L$h�T$l���d$H�WD  �;|$w`�D$L�t$`�\$d�L$h�T$l�L$P�T$T�\$X�t$\��  �o�O�W�_�w�@��o�L$H�D$L�t$`�\$d�L$h�T$l���d$H��C  ��D$H������D$;|$wy�D$L�T$p�T$P�T$`�T$T�T$d�T$X�T$h�T$\�T$l�  �w�_�P�2�_�t$H�T$L�T$p�t$`�t$P�D$d�D$T�t$h�t$X�D$l�D$\�D$��x  ���d$H�XC  ��;|$w&�3�E�D$\�E�D$X�E�D$T�E�D$P�m �[���vC  �����T����D$;|$��   �A4��   �t$��P  �w�G  �w�w�G   �G  �q�w�q�w�q�w �q �w$�q$�w(�q(�w,�q,�w0�q0�w4�o8�_<�0�_�o�t$H�D$L�D$��d  ��@�d$H��B  ���;|$w�   ����B  ���������D$;|$��   �AD��   �t$��  �w�G  �G   �G   �G   �G   �G   �w�w �G$�  �q8�w(�q@�w,�o0�W4�_8�0�_(�o�Q<�t$H�D$L�D$��  ��@�d$H�B  �;|$w�   ���B  ���������D$;|$w>�AH��   �G   �G  �o�_�0�_�o�t$H�D$L�D$��l  ���d$H�A  ��������D$;|$w>�AP��   �G   �G  �o�_�0�_�o�t$H�D$L�D$��  ���d$H�`A  ���@����D$;|$wA�  �o�O�G�   �O�O�2�o�t$H�T$L�   �   �D$���	  ���d$H�	A  ���������D$;|$��  �\$P�L$T�l$X�  �D$X�G�L$P�	�O�\$P�K�G�  �l$T�o�_�_�i�o�q�w��G �G$  �_�_(�W,�G0�   �W4�A�pL�t$\�\$\�S�T$`�t$`�n�l$d�T$d�R�Z$�G8�  �(�o<�p�w@�h�oD�p�wH�h�oL�p �wP�h$�oT�p(�wX�h4�o\�p8�w`�h<�od�p@�wh�o(�ol�s�wp�k�ot�s�wx�l$d�m�o|Ǉ�   �  �p,���   �+���   �r���   �sǇ�   �  �.���   �j���   �l$d�m ���   �n���   �v���   Ǉ�   �  �*���   �pH���   �j ���   Ǉ�     �p���   �h���   �p���   �h0���   �pD���   �C���   �j���   �r���   �B���   �j���   �r���   �t$d�F���   ����   �i���   �q���   �D$P� ���   �L$`�	���   �T$\���   �k��  �s��  �G<��  ���   ��  ���   ��  ���   ��  �\$d�s����   �o4�D$H�t$L�L$T�T$X�t$���  ��   �d$H�:>  ���������D$;|$��   ���  �o��O�K�G�  �Y@�_�W�o�o�A<�G�Q8�W �G$�  �w(�Y4�_,�o�o0�G4	  �1�w8�A�G<�Q�W@�Y�_D�i�oH�q�wL�A�GP�Q �WT�Y$�_X�i(�o\�q,�w`�A0�Gd�QH�Wh�YL�_l�iP�op�qX�wt�A\�Gx�Q`�W|�qD��_8�o(�QT�I�D$H�t$L�t$���  �ǀ   �d$H�4=  �������D$;|$��  �L$P���  �o��O�G  �W�W�k�o�s�w�K�O�S�W �k�o$�S�jD�J@�L$T�G(�  �L$���  �w,�t$T�v�w0�M�O4�r0�w8�J8�O<�O,�L$X�r<�t$\�G@�  �L$���  �wD�r�wH�J�OL�u �wP�J�OT�L$\�q�wX�J(�O\�m�o`�s�wd�K�Oh�\$X�_l�oD�l$`�Gp�   �Gt   �wt�t$d�Gx�  �l$���  �_|�L$d���   �\$`�s���   �l$`�M���   �t$X�^���   �O|�L$h�\$h�[Ǉ�   �   �k���   ���   �l$lǇ�   �  �t$l���   �L$T�1���   �J4���   ���   Ǉ�   �  �t$��x   ���   �l$\�m���   ���   ���   �t$pǇ�   �   �l$���!  ���   Ǉ�   �   �s���   ���   �l$tǇ�     �t$��`"  ���   �t$t���   �l$h�m���   �q���   �t$`�N���   �L$`�i���   �l$T�u���   ���   ���   Ǉ�     �l$���%  ���   �L$t���   �N���   �n���   �N��   �n��  �l$T�M��  ��  ���   �L$xǇ  �   �k$��  Ǉ    �J��  �j��   �l$\�M ��$  �j��(  �L$x�I��,  �l$x�m��0  �L$h�I��4  ��8  ��  Ǉ<    �l$���>  ��@  ��D  ǇH  �  �l$���>  ��L  ���P  �j��T  �Z ��X  �j$��\  �\$`��`  ��d  ��L  Ǉh    �\$���O  ��l  �R,��p  �T$T�Z��t  ��x  Ǉ|  �  ����  �]���  �P���  �X���  ����  �Y���  �P���  �X���  �P���  �X���  �P���  �Y���  �I���  �P ���  �X$���  �H(���  �P,���  �]���  �H0���  �\$P���  �W���  �L$X���  �T$`���  �\$d���  �L$h���  �T$p�Z���  �\$l���  �L$p���  ���   ���  �T$t���  ���  �\$x���  Ǉ   �   ��  ��  ��@  ��  ��  ��l  ��  Ǉ  �   ���  ��  �p4��  �P@�H<�X8��   ���7  ����D$H��d����D$;|$��   �D$L�T$P��  �P�W�P�W�w�_�O�p��_�H�T$H�t$L�T$P�t$��<  ���d$H��D$H�� ����D$;|$w!�D$L�h�@�D$L�D$���  �D$H�g�����6  ���������D$;|$w�1�t$H�L$L�͋D$��l  �d$H�6  �;|$wD�ÉL$P�T$T��u�p�H�)�H�X�T$T���*�H�X�p�l$H�T$L�T$T�l$P�d$H�d6  ��D$H��D����D$;|$�}   �D$L�l$P�  �w�_�O�W�G  �H�O�P�W�X�_ �h�o$�p�w(�H�O,�P(�W0�_�_4�h�u �_�P$�H �t$H�l$L�l$P�D$��X  ��8�d$H��5  ���������D$;|$��   �ÉL$P����  �\$���  �W�o�p�w�G  �P�W�X�_�p�w�P�W �X�_$�o(�h�u�w,�P�W0�1�_�o��t$H�L$L�L$P�D$���  ��8�d$H�,5  ��D$H������D$;|$w]�D$L�D$P�T$T�T$P�R��  �j�o�B�G�w�_�O�r��_�L$P�i�J�D$H�t$L�T$T�t$��l  ���d$H��4  ���������D$;|$w�1�t$H�L$L�͋D$���  �d$H�4  �;|$wD�ÉL$P�T$T��u�p�H�)�H�X�T$T���*�H�X�p�l$H�T$L�T$T�l$P�d$H�44  �������D$;|$�|   ��l$\�   �  �D$���  �O��O�S�W�k�o�C�G�K�O�.�G�S�K�[�l$H�t$L�l$\�l$P�D$T�D$X   �=   �t$��T  �� �d$H�3  ����D$H��|����D$;|$wn�D$L�l$X�l$P�l$\�  �h�o�h�o�w�_�O�W�G  �p�w �O�O$�h�u �_ �P�t$H�l$L�L$X�l$\�D$��  ��(�d$H�+2  ��������D$;|$w)�l$T�3��j�D$H�t$L�D$T�D$P�t$��L  �d$H��2  ���������D$;|$w)�L$T��1�B�D$P�t$H�L$L�L$T�t$���  �d$H�2  ����t����D$;|$w#�3��D$H�t$L�L$P�   �t$���  �d$H�[2  ��<����D$;|$w.��1�B�D$P�[�t$H�L$L�   �   �t$��  �d$H�2  �;|$wA��l$T��D$\�L$\�	�L$X�n�V�N�^�v�D$X�D$H�D$\�D$L�D$T�D$P�d$H��1  �������D$;|$w$�Ë�݋L$P�(�l$H�D$L��t$���  �d$H�0  ���;|$w�\$T���ڋ3�S�K�[�l$T�D$P���g1  �D$H��H����D$;|$w[�D$L�L$P�T$T�P�*�
��pH�
�  �H�O�P�W�w�_�p��_�D$H�t$L�L$P�T$T�t$��0   ���d$H�	1  ΐ��;|$w:��L$T�T$X�l$\���V�j�^�v�L$H�D$L�L$T�T$X�D$\�D$P�d$H�0  ����D$H�������D$;|$��   �D$L�t$T�\$X�p�  ��_�o�^�_�n�o�\$T�_�l$X�o�O�W ��(�W܋X��u,��B�0�)�l$P�j�I�[�t$H�D$L�t$��P!  �d$H��  ��G�G   �J�O�T$��o�\$H�T$L�   �   �   �t$���   ���d$H�������D$;|$w"�����t�n�V�N�^��������T����/  �/  �D$H��t����D$;|$w��   �D$���!  �o�o�����u/  �D$H��@����D$;|$�}   �t$X�t$P�D$T��  �w�G�t$X�w�_�O���G�݃�u�p�H�X�   ���1�h�t$H�L$L�ȋD$��<"  �d$H����������D$;|$w��p�����-  ��.  ΋D$H�������D$;|$w"�D$L�  �l$���"  �o�G�o�����.  ��D$H��d����D$;|$��  �D$L�t$T�\$X�t$P�@�  �X�_�X�_�X�_�X�_�X�_�o�h�o�@�G �D$T�G$�\$X�_(�O,�W0�_�΃�8;|$�w  ������   ��q�
�j�l$T�h;�u�h �P,�H(�X$���l$T��} u8�9u-�X��I�L$P�m�T$H�\$L�\$T�΋Ћt$��0$  �d$H�΋�넃9u-�P��I�L$P�m�\$H�T$L�\$T�΋Ћt$��`$  �d$H�΋��I����  ��O�P�W�X�_�h�o�_�p�>u+���P�H�F�1�j �t$H�L$L�D$P�D$���$  �d$H���P�H�F�Ӑ��������D$;|$wR�Ëڃ�������s �S,�K(�[$���搐��������D$;|$w"�Ëڃ�������s �S,�K(�[$�����_+  �,  �ӻ   �   �   �D$H   �D$L   �T$ �l$��D����\$���E��������,����D$;|$w)�1�B�D$P�t$H�L$L�   �   �t$��%  �d$H�,  ���������D$;|$w.���S��k�K��D$H�T$L�t$P�   �t$��X%  �d$H��+  ��������D$;|$wD��  �_�G   �O�L$��o�T$H�L$L�   �   �   �t$���   ���d$H�n+  ����D$H��L����D$;|$w"�D$L�  �l$���%  �o�G�o�����F+  ��D$H������D$;|$��  �D$L�t$T�\$X�t$P�@�  �X�_�X�_�X�_�X�_�X�_�o�h�o�@�G �D$T�G$�\$X�_(�O,�W0�_�΃�8;|$�w  ������   ��q�
�j�l$T�h;�u�h �P,�H(�X$���l$T��} u8�9u-�X��I�L$P�m�T$H�\$L�\$T�΋Ћt$���'  �d$H�΋�넃9u-�P��I�L$P�m�\$H�T$L�\$T�΋Ћt$���'  �d$H�΋��I����  ��O�P�W�X�_�h�o�_�p�>u+���P�H�F�1�j �t$H�L$L�D$P�D$��((  �d$H���P�H�F�Ӑ���|����D$;|$wR�Ëڃ�������s �S,�K(�[$���搐���L����D$;|$w"�Ëڃ�������s �S,�K(�[$�����(  �4)  �ӻ   �   �   �D$H   �D$L   �T$ �l$�������\$���E�������������D$;|$w)�1�B�D$P�t$H�L$L�   �   �t$��h(  �d$H�(  ���������D$;|$w.���S��k�K��D$H�T$L�t$P�   �t$���(  �d$H�t(  ���T����D$;|$wD��  �_�G   �O�L$��o�T$H�L$L�   �   �   �t$���   ���d$H�(  ����������D$;|$�P  �L$`�  �J�O�j�o�r�w�_���w��J�Z�\$d�T$h�T$��<H  �l$l�T$h��  ��_�j�o�Z�_�j�o�G�B�G�Z�_�j�o �B�G$�G�֋\$d���(;|$��  �T$d�T$`�����g  �t$��@Q  �2���4��0���B�  �t$l�w�T$d�W�_�o�w��\$`�X�֋���L$��.  �t$l��닋R�  �D$l�G�t$d�w�_�o�G��\$`�Z�Ћ���t$���-  �t$l���F�����  ��G�D$l�G�t$d�w�_�o�G�  �Y�_�i�o �q�w$�A �G(�_�_,�q�.�_�R�I�l$H�t$L�   �t$��4  ��0�d$H��  ��G�D$l�G�t$d�w�_�o�G  �Y�_�i�o �q�w$�G�G(�w�B�D$`�i �Y�֋��L$���5  �L$l��0�n����r�  �T$l�W�D$d�G�_�o�o��T$`�^�Ջ���t$��@-  �t$l���)�����  ��G�D$l�G�t$d�w�_�o�G  �Y�_�i�o �q�w$�G�G(�w�B�D$`�i �Y�֋��L$���9  �L$l��0������r�  �T$l�W�D$d�G�_�o�G��T$`�^�Ћ���t$���,  �t$l���|�����  ��G�D$l�G�T$d�W�_�o�G�  �Q�W�Y�_ �o�o$�A��w�Q �I�\$H�D$L�޽   �t$�� >  ��(�d$H��D$H��l����D$;|$��  �D$L�D$p�t$l��L$d�ʉl$`�T$P�T$t�l$p�]�\$h�D$t������ ����D$;|$�x  ���  ��W�h�o�p�w�P�W�h�o�_�W�h �X�L$`�t$��L;  �t$l�� �j�����������D$;|$�  ���  ��W�n�o�F�G�V�W�n�o�_�G�n �^�ЉL$`�Ƌt$��H7  �L$l�� �������\����D$;|$��   �Ë��  ��W�^�_�n�o�V�W�^�_�G�G�n �^�ЉL$`�Ƌt$��\1  �t$l�� ��������������D$;|$wP���  ��W�h�o�p�w�P�W�h�o�_�W�h �X�L$`�t$���.  �L$l�� �B����b"  �"  �L$l�t$`�D$H�D$L   �T$ �D$��w����D$�L$l�t$`�D$H������X����D$;|$w�)�l$H�L$L�   �t$���.  �d$H�E"  ����$����D$;|$w!���͋(�l$H�D$L�   �t$��/  �d$H�"  ���������D$;|$w<�L$T�  �o�_�K�1�_�B,�D$P�t$H�L$L�L$T�t$��h/  ���d$H�!  ����������D$;|$w,�L$T�s�N��3�t$P�D$H�L$L�L$T�D$���/  �d$H�v!  �����T����D$;|$w-�L$T�s�N��r(�t$P�D$H�L$L�L$T�D$���/  �d$H�1!  ��������D$;|$w"�C�p��D$H�t$L�L$P�t$��(0  �d$H��   ��������D$;|$w_��K��  ��G��_�A�G�G�  �Y�_�w�w�w�q��_�R�T$P�Q�I�D$H�t$L�t$���0  �� �d$H�   ��d����D$;|$w*����D$H�T$L��t$P�   �   �t$���0  �d$H�D   ���$����D$;|$w*���k�[�T$H�L$L�   �   �t$��1  �d$H�   �;|$w5��L$T�s�D$T��L$P�k�V�N�^�6�D$P�D$H�D$T�D$L�d$H��  �������D$;|$w�)�l$H�L$L�   �t$���1  �d$H�  ����p����D$;|$w!���͋(�l$H�D$L�   �t$���1  �d$H�Y  ����8����D$;|$w<�L$T�  �o�_�K�1�_�B,�D$P�t$H�L$L�L$T�t$��2  ���d$H�  ����������D$;|$w,�L$T�s�N��3�t$P�D$H�L$L�L$T�D$��`2  �d$H��  ����������D$;|$w-�L$T�s�N��r0�t$P�D$H�L$L�L$T�D$���2  �d$H�}  ����\����D$;|$w"�C�p��D$H�t$L�L$P�t$���2  �d$H�D  ���$����D$;|$w_��K��  ��G��_�A�G�G�  �Y�_�w�w�w�q��_�R�T$P�Q�I�D$H�t$L�t$��P3  �� �d$H��  �������D$;|$w*����D$H�T$L��t$P�   �   �t$���3  �d$H�  ���p����D$;|$w*���k�[�T$H�L$L�   �   �t$���3  �d$H�P  �;|$w5��L$T�s�D$T��L$P�k�V�N�^�6�D$P�D$H�D$T�D$L�d$H�  �������D$;|$wA�ÉL$T�  �o�G�H�1�_�@�@,�D$P�t$H�L$L�L$T�t$��h4  ���d$H�  ���������D$;|$w#���͋(�l$H�D$L��   �t$���4  �d$H�  ��`����D$;|$w0���C�P�*�l$H�T$L��t$P�   �   �t$���4  �d$H�:  ���������D$;|$wD�K�  ��W�Y�_�q��_�Q�R�T$P�Q�	�D$H�t$L�t$��D5  ���d$H��  ����������D$;|$w*����D$H�T$L��t$P�   �   �t$���5  �d$H�  �;|$w5�s��D$T�L$T�	�L$P�+�V�N�^�v�D$P�D$H�D$T�D$L�d$H�[  ��<����D$;|$w"�)�l$H�L$L�   �   �t$���5  �d$H�$  �������D$;|$w$�K�1�B4�D$P�t$H�L$L�͋t$��86  �d$H��  ����������D$;|$w�s��D$H�t$L�L$P�t$��l6  �d$H�  �������D$;|$wG�Él$T�  �O�H�O�h�u �_�J�L$P�P��t$H�l$L�l$T�t$���6  ���d$H�W  ��8����D$;|$w*����D$H�T$L��t$P�   �   �t$��7  �d$H�  �;|$w5�s��D$T�L$T�	�L$P�+�V�N�^�v�D$P�D$H�D$T�D$L�d$H��  �������D$;|$w�)�l$H�L$L�   �t$��|7  �d$H�  ���������D$;|$w!���͋(�l$H�D$L�   �t$���7  �d$H�m  ����L����D$;|$w<�L$T�  �o�_�K�1�_�B,�D$P�t$H�L$L�L$T�t$��8  ���d$H�  ����������D$;|$w,�L$T�s�N��3�t$P�D$H�L$L�L$T�D$��L8  �d$H��  ����������D$;|$w-�L$T�s�N��r8�t$P�D$H�L$L�L$T�D$���8  �d$H�  ����p����D$;|$w"�C�p��D$H�t$L�L$P�t$���8  �d$H�X  ���8����D$;|$w_��K��  ��G��_�A�G�G�  �Y�_�w�w�w�q��_�R�T$P�Q�I�D$H�t$L�t$��<9  �� �d$H��  �������D$;|$w*����D$H�T$L��t$P�   �   �t$��|9  �d$H�  ��������D$;|$w*���k�[�T$H�L$L�   �   �t$���9  �d$H�d  �;|$w5��L$T�s�D$T��L$P�k�V�N�^�6�D$P�D$H�D$T�D$L�d$H�#  ������D$;|$w"�)�l$H�L$L�   �   �t$��4:  �d$H��  ��������D$;|$w$�͋s��j<�D$H�t$L�L$P�t$��p:  �d$H�  ����������D$;|$wG�Él$T�  �O�H�O�h�u �_�J�L$P�P��t$H�l$L�l$T�t$���:  ���d$H�S  ��4����D$;|$w*����D$H�T$L��t$P�   �   �t$��;  �d$H�  �;|$w5�s��D$T�L$T�	�L$P�+�V�N�^�v�D$P�D$H�D$T�D$L�d$H��  �������D$;|$w�)�l$H�L$L�   �t$���;  �d$H�  ���������D$;|$w!���͋(�l$H�D$L�   �t$���;  �d$H�i  ����H����D$;|$w<�L$T�  �o�_�K�1�_�B,�D$P�t$H�L$L�L$T�t$��<  ���d$H�  ����������D$;|$w,�L$T�s�N��3�t$P�D$H�L$L�L$T�D$��P<  �d$H��  ����������D$;|$w-�L$T�s�N��r@�t$P�D$H�L$L�L$T�D$���<  �d$H�  ����l����D$;|$w"�C�p��D$H�t$L�L$P�t$���<  �d$H�T  ���4����D$;|$w_��K��  ��G��_�A�G�G�  �Y�_�w�w�w�q��_�R�T$P�Q�I�D$H�t$L�t$��@=  �� �d$H��  �������D$;|$w*����D$H�T$L��t$P�   �   �t$���=  �d$H�  ��������D$;|$w*���k�[�T$H�L$L�   �   �t$���=  �d$H�`  �;|$w5��L$T�s�D$T��L$P�k�V�N�^�6�D$P�D$H�D$T�D$L�d$H�  �� ����D$;|$w&�1�BD�D$P�t$H�L$L�ͺ   �t$��<>  �d$H��  ��������D$;|$w+�ŋs��+�[�T$H�t$L�D$P�   �t$��|>  �d$H�  ;|$w.��L$P���L$T�V�N�^�v�l$T�l$H�D$L�l$P�d$H�j  ����D$H��H����D$;|$w%�D$L�  �l$���,  �o�@�G�o�����?  ���D$H������D$;|$��  �D$L�t$T�\$X�L$\�l$`�t$�X�h��  �L$���@  �O�M�O�M�O�w�s�w�L$T�O�t$X�w�L$\�O �W$�w�t$�G(�  �H�O,�P�W0�M�O4�U�W8�H�O<�P�W@�E �GD�wH�_L�u��_,�U(�M$�D$H�t$L�l$`�t$���@  ��P�d$H��D$H��(����D$;|$��   �D$L�L$`�L$P��uE�@��  �h�o�w�_�_�)�p�t$L�L$`�D$���O  �D$���>  �D$H�������h��  �M�O�w�_�u��_�M�i�M���   �D$H�t$L�L$`�t$���O  ���d$H�]  �  ���;|$wI�D$L�H�L$�h�l$T�L$T�1�t$X�X�SH�T$P�h�P �H�X�p�D$X�D$H�D$T�D$L�d$H�E  ������D$;|$w@������  �t$��   �o�O�W��o�T$H�D$L�   �t$��HA  ���d$H��  ����������D$;|$��   �ÉL$\�l$`�H�   �p �H�  �\$��<B  �W�n�o�Q�W�Y�_�n�o�V�W�G�  �Y�_ �i�o$�Q�W(�Y�_,�i �o0�G4  ��O8�P�W<�X�_@�h�oD�wH�w �wL�p��_8�o�P�L$H�t$L�L$\�D$`�D$P�l$T�D$X   �=   �t$��D  ��P�d$H��  ��D$H��Ľ���D$;|$wn�D$L�l$X�l$P�l$\�  �h�o�h�o�w�_�O�W�G  �p�w �O�O$�h�u �_ �P�t$H�l$L�L$X�l$\�D$���B  ��(�d$H�s  ���<����D$;|$w)�l$T�3��j�D$H�t$L�D$T�D$P�t$��C  �d$H�  ���������D$;|$w)�L$T��1�B�D$P�t$H�L$L�L$T�t$��DC  �d$H��  ���������D$;|$w#�3��D$H�t$L�L$P�   �t$��|C  �d$H�  �������D$;|$w.��1�B�D$P�[�t$H�L$L�   �   �t$���C  �d$H�`  �;|$wA��l$T��D$\�L$\�	�L$X�n�V�N�^�v�D$X�D$H�D$\�D$L�D$T�D$P�d$H�  �������D$;|$w#���͋2�t$H�T$L��   �D$��DD  �d$H��  �������D$;|$wJ�  ��W�k�o�s�w�C�G�S�s��_�j���   �D$H�t$L�t$���D  ���d$H�|  ���\����D$;|$wB�L$T�  �o�_�r��_�J���   �L$P�D$H�t$L�L$T�t$���D  ���d$H�$  �������D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$��<E  �d$H��  ���ĺ���D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$��|E  �d$H�  ��������D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$���E  �d$H�d  ���D����D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$���E  �d$H�$  �������D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$��<F  �d$H��
  ���Ĺ���D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$��|F  �d$H�
  ��������D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$���F  �d$H�d
  ���D����D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$���F  �d$H�$
  �������D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$��<G  �d$H��	  ���ĸ���D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$��|G  �d$H�	  ��������D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$���G  �d$H�d	  ���D����D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$���G  �d$H�$	  �������D$;|$w*�r��j���   �l$P�+�D$H�t$L�t$��)  �d$H��  ���ķ���D$;|$w"�)�l$H�L$L�   �   �t$��tH  �d$H�  ��������D$;|$w'�͋2�C���   �t$H�T$L�L$P�t$���H  �d$H�o  ��P����D$;|$w*��s���   �t$P�D$H�T$L�   �D$���H  �d$H�0  �������D$;|$w,�Ջs��k�m �D$H�t$L�T$P�   �t$��4I  �d$H��  �����̶���D$;|$w'���3�.�S�K�[�l$H�t$L��t$��pI  �d$H�  �������D$;|$w$�3���   �D$P�*�t$H�\$L�t$���I  �d$H�v  �����T����D$;|$w$�3���   �D$P�*�t$H�\$L�t$���I  �d$H�:  ���������D$;|$w$�3���   �D$P�*�t$H�\$L�t$��$J  �d$H��  �����ܵ���D$;|$w$�3���   �D$P�*�t$H�\$L�t$��`J  �d$H��  ����������D$;|$w$�3���   �D$P�*�t$H�\$L�t$���J  �d$H�  �����d����D$;|$w$�3���   �D$P�*�t$H�\$L�t$���J  �d$H�J  �����(����D$;|$w$�3���   �D$P�*�t$H�\$L�t$��K  �d$H�  ���������D$;|$w$�3���   �D$P�*�t$H�\$L�t$��PK  �d$H��  ����������D$;|$w$�3���   �D$P�*�t$H�\$L�t$���K  �d$H�  �����t����D$;|$w$�3���   �D$P�*�t$H�\$L�t$���K  �d$H�Z  �����8����D$;|$w$�3���   �D$P�*�t$H�\$L�t$��L  �d$H�  ����������D$;|$w!�3�A|�D$P�*�t$H�\$L�t$��<L  �d$H��  ����ĳ���D$;|$w!�3�Ax�D$P�*�t$H�\$L�t$��tL  �d$H�  ���������D$;|$w!�3�At�D$P�*�t$H�\$L�t$���L  �d$H�u  ����T����D$;|$w!�3�Ap�D$P�*�t$H�\$L�t$���L  �d$H�=  ��������D$;|$w!�3�Al�D$P�*�t$H�\$L�t$��M  �d$H�  ��������D$;|$w!�3�Ah�D$P�*�t$H�\$L�t$��TM  �d$H��  ���������D$;|$w!�3�Ad�D$P�*�t$H�\$L�t$���M  �d$H�  ����t����D$;|$w!�3�A`�D$P�*�t$H�\$L�t$���M  �d$H�]  ����<����D$;|$w!�3�A\�D$P�*�t$H�\$L�t$���M  �d$H�%  ��������D$;|$w!�3�AX�D$P�*�t$H�\$L�t$��4N  �d$H��  ����̱���D$;|$w!�3�AT�D$P�*�t$H�\$L�t$��lN  �d$H�  ���������D$;|$w!�3�AP�D$P�*�t$H�\$L�t$���N  �d$H�}  ����\����D$;|$w+�3�AL�D$P�*�t$H�\$L�   �   �t$���N  �d$H�;  ������D$;|$w.�Z��)�C�
�0�S�[�t$H�D$L�L$P�t$��(O  �d$H��  ���ذ���D$;|$w$�+�l$H�\$L��   �   �t$��dO  �d$H�  ���;|$w��L$�p�P�H�X���  ����D$H��x����D$;|$w"�D$L�  �l$���?  �o�G�o�����r  �;|$w
�s�+�[���F  ���;|$w
�s�+�[���.  �  �D$P�G�O�O���T$ ��D$P�I�d$H�D$P�D$H�D$L   �T$ �D$H�D$P����  �D$P�G�D$T�G�O�O���T$ ��D$P�A�D$T�I�d$H��  �D$P�G�D$T�G�D$X�G�G���D$H�D$\�D$L�T$ �D$H�D$`�D$`� �D$P�D$`�@�D$T�D$`�@�D$X�D$L�D$\����  �D$P�G�D$T�G�D$X�G�D$\�G�O�O���T$ ��D$P�A�D$T�A�D$X�A�D$\�I�d$H�D$H   �D$L   �T$ ���T$ �d$H�����������#�����������@�������calc.sml   1p�calc"5DCA nff9pa"regNum"4��nC"ref"���nC�int"0 pa"regList"4a�1a�nC"list"1aŲ�1"�2"2��B�Y��9�JY�u�*c" �'0i1��+0��pa"regStack"4a�1������B��  ��nC�string"00i1b�� 0p�emptyRegStack"5c��3t��nC"exn"0�DNfsCpa"pushReg"4a�nC"->"2����C�unit" ��000����50pa"popReg"4a��'2��<��pa"createReg"4a��'2���������'�'a��2����3p�unknownReg"5c��t��	��Nfsp�unFreedReg"5c��,t��	��NfsCpa"unFreedCount"4d�fa��'���������� �'�'	pa"freeReg"4a��'2a��2������������Bp"�������b���" ��0i2��6d"Int31Imp"0�'��
pa"getReg"4���'���<��pa"delReg"4a��'2����<pa"offsetRegs"4d��a��'2a����#��$Cpa"concatRegs"4d��a��'2a��2��$��$��$p�tooComplex"5c��t��	��Nfspa"pprintRegs"4a��'2a��1a��2������B�� ��20��0pa"printRegs"4a��'2a��2����nB�)#��3�G���hQ�B"0��<p�add'"5c��f���'�������nBp"�4+ݸB�F�[!��P;"0��)��)� �fnCp�div'"5c��f�����
fnp�integer'"5c��)fa��'2����)���
fnp�negate'"5c��f���'��(���
fnp�prod'"5c��,f�����
fnp�recall'"5c��t��)� ��
fnCp�store'"5c��!f�����
fnp�sub'"5c��;f�����
fnp�AST"1b�� �p�calcLrVals"2BnB����%HfV� �+џ"n�p�calcLex"2BnB𢋙(����Ӓ��{��]"n�Cp�calcParser"2BnB���]�S�����%{���"9n�pa"input_line"4a��'2a�nB��8	0��pa"calcparse"4a��'2��a��2a�A a�A ��)0i5C�result"�ParserData"�<returnStr>"��"�N00i4��*�� ��4�0a�B�{�j������΋8"cBB�J��� �i(��!v�"cCB�� a�1aB��� b na1��$bB�� 1c�str"2da"EVAL"��sa��2��.a�1a� ���.da"UNEVAL"��!sa��'2��<��.f� 0i2��;�Stream"���t��stream"����i2���LrParser"1a����' b na1��8bB��$1c�token"1da"TOKEN"Asa��2�� 0aų�����3"3��.���= f� 1����'  b na1��
bB�� ���term"���T"A��'  f��0i3��!�LrTable"��*i3���Token"��*2a�A ��A	 a�A
 a�A a��  b na1��2bB��3&1c�svalue"9da"Expr"�����)da"Factor"��!��da"Int"����/da"NegFactor"��$��Cda"Prog"��=��da"StoreIt"��3��da"Term"����da"VOID"��nda"ntVOID"Bs��<N  fS0i6����MlyValue"��3�� ��"�N00i5C����3�� ��"�N00i5C���Tokens"�� ��"�N00i5C���UserDeclarations"�� ���N00i4���� ��4�0a�A ��A ��0i5C�pos"��4�� ���N00i4��1�� ��4�0pa"label"4�pa"nextLabel"4��Cpa"relOpOpposites"4a��1a��2����p�notLocated"5c��t��	��Nfspa"opposite"4a��'2����pa"forloop"4d�ffa��'2a��-3�'a��'���.��=��.�'p�unboundId"5c��>t��	��NfsCp�constant'"5c��f���'������  b na1��8bA1c�Type"2d�������d�function'"��!��  f��=0i2��	�0����=fnp��5c��f��.��!��=fnp��	1b��	 �pa"boundTo"4d��a��'2������6�������-���6��.��=��.pa"depthOf"4d��a��'2��=��=Cpa"frameSize"4�' p�Unimplemented"5c��t��	��Nfs!pa"codegen"4d3fffa��'2aE5C������5h"4"h"5"N5C��)��4��.��=bN��2"pa"compile"4a��'2����#pa"run"4d��a��'2a��2��.��aCBp"�PW�iJg3w�bD�" aCBp"���U6'��D݇�.�" ��0i3�status"d"Process"d"OS"00i2��d"OS_Process"0$Nn00sAEAA5Cp�����*p��	Bs�EAA3pB��3����AA9����3	����"�AA2pB��3����AA3pB��8��'%�ь�6�"��Aa��7��0�tti5C������ ��"�NpB�����'����3 �����AA1pB��3"��-EA A1pB��3!���A!A1pB��3
Bs��1EA"A2pB��3�����'���������	����������'	 b na1��6bB�����pairlist"���EMPTY"��n��PAIR"A����-���.��=�����0 f�0i3����>��*���������'
  b na1��bB�����state"���STATE"A��/  f��0i3��3��>��*���������'  b na1��#bB��!���nonterm"���NT"A��/  f��0i3��:��>��*���������'  b na1��)bB��"1c�action"4��ACCEPT"��n��ERROR"�n��REDUCE"����/d�SHIFT"��!s��(  f�1��i3�� ��>��*������B��' cCB𢉊� B6�G�����" aE5Ch��h"goto"h"initialState"h"rules"h"states"N5Ca�B��aAnC�array"����i1���>1a��2a����+ b na1��bB��+��0i2����>2aB���+  b na1��=bB��+��0i2��!��>0�����+
  b na1��!bB��+1c�� 4��
����$d��5��!s�����+  b na1��bB��+��10i2��3��>0  f��0i2�� ��>0��a��31a��2aB���+  b na1��bB��+	��80i2��:��>0��������N0i2�table"��>i2����*NB���>��*pB��3�����'���������7������B�����*EA#A1pB��3Bs��A$A1pB��3Bs�EA%A6pB��3��4CpB��3��pB��3��)pB��3��!pB��3��'pB��3��N�A&A1pB��3���EA'A1pB��3���,EA(A1pB��3BsA)EA*A1pB��3A��1�A+A1pB��3��A, ��0i5C�arg"��3�� ��"�NEA-A1pB��3��A. ��0i5C��1��3�� ��"�NEA/A1pB��3��%EA0A1pB��3��EA1A1pB��3BsA2EA3A1pB��3���A4A1pB��3 BsA5EA6A1pB��3��%B�0Bi5Cd"Header"��3�� ��"�N��)i5��&��!i5C�EC"��3�� ��"�N��%i5C�Actions"��3�� ��"�N��i4��3�� ��"���)����� ��"���?������?��� ��"���������������?�������,C�������0�����	�������
��%����������A7A7��8��C����'��2��<��N��)�����3pB���Bs��&EA8A8��8����C��'��2��<����N��)i2��0��3N��)���3pB��3����8��)��p��0Bs�EA9A2pB��ACA: a��'2a��2a��; b na1��8bB��1c��1da��AsaE5Ch"id"h"lastWasNL"h"lineNo"h��2h"strm"N5C�'aAnC"bool"0�'�'a�nB��80N f��0i5C���yyInput"�� ���N0a��< b na1��bB��#1c�yymatch"2da"yyMATCH"Asa��-3��(a��'2a��2��(���0aG2aG0�'��da"yyNO_MATCH"��n f��3��7��.��'i4���� ���0a�A= a�A>a��.��0��9i5C����4�� ���N2��!��%0i5Cb"lexresult"��4�� ���N00i4�� �� ���pB��A�� EA?A1pB��Bs�� EA�A1pB��A��EA�A1pB�����
EA�A1pB�����"EA�A1pB�����&�A�A1pB��AB��  b na1��bB��1c�yystart_state"1da"INITIAL"��n  fS 0i4���� ���EA�A2pB��Bs��EA�	A2����	�����A�
A1pB�������8������<�5ͩ����u[�="��B��8 ��0i3�vector"�StreamIO"�TextIO"������B��8 ��B�� aAnC�char"00i1b��00i3�elem"��1��<������B��8 a�nB�U1���gd�aӒxI�"00i3b"reader"��1��<�������B��8 a�nB��	00i3b"writer"��1��<�������������nB��8������B��8 a�B�� aCB�� aCBp"THэ�I�����f�0��" �'0i2��6d"Int31"00i1��100i3��1�� �TextPrimIO"00i3��1��1��<������nB��8NB���1��<EA�A1pB��
���7EA�A1pB��BsB�� A8pB𢹣Q�U 
���sC= �"ACB�� aCB�8u��^�q�J�����" ��0i2��1�CharVector"00i3��1�� ��pB��)ACB�� aCB�� ��0i2��(��&00i3��(�� ��pB��)ACB�� aCBp"V^���g˭��} " aAnBp"�i +�3�����"00i2b"slice"d"CharVectorSlice"00i3b"vector_slice"�� ��CpB��)ACB�� aCBp"�[�|�	�s��Y唛�" aBB�@A���+σ��=���"/  cCB��0 aCBp"�kU8�.�~]��?�" aBB�g��#�Z*����"  cCB�� aBC�word8array" ai1���(00i3��:�A"�Assembly"i2��:��	00i4��:����	d"Core"00i3���CharArray"d"InlineT"�����700��00i3���� ��pB��)ACB�� aAnBp"�]$G��x7��pҷ��"00i3b"array_slice"�� ��pB��)A��pB��)A��pB��)A��NDnB��
i2�� ��EA�A1pB����;EA�A1pB��BsB��8A5CpB�蝉<L1�W˄�.!��x"ACB��8 ��0i2��(��<pB��:ACB��8 ��0i2��1��<pB��:A��2pB��:A��5pB��:��;NBi1��<�A�A1pB�� BsA�EA�A1pB����B��B���+�� ���pB������ ����$�� i4��4�� �����6i3�� ���p��Bs�EA�A7pB𢠄ܝ���	�~@��	"	Bs��EA�A2pB����%pB�������'���������%B�����*EA�A1pB�����EA�A1pB�����EA�A1pB�����!EA�A1pB����A� a��-00i4��8�� ��4�EA�A1pB����?EA�A1pB��
��%�A�A1pB�� BsA�EA�A3pB��Bs��EA�A1pB��-Bs��3EA�A3pB��.��pB��/��"pB��0��2Bi2��4�Lex"Bi1��(pB����/pB��BsB��'A3pB��6��?pB��7��pB��8��%Bi1��*B��Bi3�� ��4�pB��:��%CpB��;��?pB��<��pB��=��7pB��>��pB��?��'N����p��%A��7NBi1�A 7�B��C����������C����������C����������C���0������C����������C����������C����������N