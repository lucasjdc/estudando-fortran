#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct{
	char*name;
	double accuracy,speed,lucky; /* attributes */
	int alive,lethal,quick,luck; /* conditions */
}

gunslinger;

gunslinger g1={"Blondie",0.75,0.75,0.75,0,0,0,0}; /* fast, accurate & lucky */
gunslinger g2={"AngelEyes",0.50,0.50,0.50,0,0,0,0}; /* all round average */
gunslinger g3={"Tuco",0.50,0.75,025,0,0,0,0}; /* fast but unlucky */

void Draw(gunslinger*man){
	/* randomly set speed, lethality, and luck based on attributes */
	if(man->alive){
		man->quick=(int)(man->speed*rand());
		man->lethal=((man->accuracy*32767)>=rand());
		man->lucky=((man->lucky*32767)>=rand());
	}
	else
		man->quick=man->lethal=man->luck=0;
}

void sort(gunslinger**guns,int n) {
/* bubble sort on draw: fast to slow */
	int i,j;
	gunslinger*g;
	do {
		for(j=i=0; i < n-1 ;i++){
			if(guns[i]->quick<guns[i+1]->quick){
				g = guns[i];
				guns[i] = guns[i+1];
				guns[i+1] = g;
				j = 1;
			}
		}
	}while(j);
}

int TwoDraw(gunslinger*man1,gunslinger*man2){
	gunslinger*draw[2];
	Draw(man1);
	Draw(man2);
	draw[0]=man1;
	draw[1]=man2;
	sort(draw,2);
	if(draw[0]->lethal)
		draw[1]->alive=0; /* g1 kill g2 */
	else if (draw[1]->lethal)
		draw[0]->alive=0; /* g2 kill g1 */
	return (man1->alive&&man2->alive);
}
void TwoWay(gunslinger*man1,gunslinger*man2){
	int shot=0;
	man1->alive=man2->alive=1;
	printf("%-9s vs. %-9s",man1->name,man2->name);
	do{
		shot++;
	} while (TwoDraw(man1,man2));
	printf(" survivor: %-9s after %i shot%s\n",man1->alive?man1->name:man2->name,shot,shot>1?"s":"");
}
int ThreeDraw(gunslinger*man1,gunslinger*man2,gunslinger*man3){
	gunslinger*draw[3];
	Draw(man1);
	Draw(man2);
	Draw(man3);
	draw[0]=man1;
	draw[1]=man2;
	draw[2]=man3;
	sort(draw,3);
	if(!draw[2]->alive) /* g2 is already dead */
		return(TwoDraw(draw[0],draw[1]));
	if(draw[0]->lethal){
		if (draw[0]->luck)
			draw[1]->alive=0; /* g1 kills g2 */
		else
			draw[2]->alive=0; /* g1 kills g3 */
	}
	if (draw[1]->alive) /* if g2 is still alive */
	{
		if(draw[1]->lethal){
			if(draw[1]->luck)
				draw[0]->alive=0; /* g2 kills g1 */
			else
				draw[2]->alive=0; /* g2 kill g3 */
		}
	 }
	if (draw[2]->alive) /* if g3 is still alive */
	{
		if(draw[2]->lethal)
		{
			if(draw[2]->luck)
				draw[0]->alive=0; /* g3 kills g1 */
			else
				draw[1]->alive=0; /* g3 kills g1 */
		}
	 }
	 return((man1->alive&&man2->alive))||
			 ((man1->alive&&man3->alive))||
			 ((man2->alive&&man3->alive));
}
void ThreeWay(gunslinger*man1,gunslinger*man2,gunslinger*man3) {
	int shot=0;
	man1->alive=man2->alive=man3->alive=1;
	printf("%s vs. %s vs. %s",man1->name,man2->name,man3->name);
	do{shot++;
	} while (ThreeDraw(man1,man2,man3));
	printf(" survivor: %-9s after %i shot%s\n",man1->alive?man1->name:man2->alive?man2->name:man3->name,shot,shot>1?"s":"");
}
int main(int argc,char**argv,char**envp){
	int duel;
	time_t t;
	time(&t);
	srand((unsigned)t);
	for(duel=1;duel<=5;duel++)
		TwoWay(&g1,&g2);
	for(duel=1;duel<=5;duel++)
		TwoWay(&g1,&g3);
	for(duel=1;duel<=5;duel++)
		TwoWay(&g2,&g3);
	for(duel=1;duel<=5;duel++)
		ThreeWay(&g1,&g2,&g3);
	return(0);
}
