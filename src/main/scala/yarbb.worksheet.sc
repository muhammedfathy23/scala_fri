import scala.annotation.tailrec
import scala.compiletime.ops.string
import scala.compiletime.ops.int
def lastodd (list:List[Int]): Int ={

var x =0
for (i<- list )
    if(i%2!=0)
      x=i

x
}

def removedupples(list:List[Int]):List[Int]={
list.distinct
}


def fristeven(list:List[Int],size:Int):Int={
var x = -1
for (i <- 0 until size )
  if (list(i) % 2 == 0) 
    x=list(i)
    return x
x
}
def sumofodd(list:List[Int]):Int={
var sum=0
for(i <- list)
  if (i % 2 !=0)
    sum+= i
sum
}

def reverse(list:List[Int]):List[Int]={

var newlist:List[Int]= Nil

for (i<-list)
  newlist = i::newlist

newlist

}

def findsecondlargest(list:List[Int]):Int={
var largest = list(0)
for(i<-list)
  if(i>largest)
    largest=i


var second=list(0)
for(i<-list)
  if(i > second && i < largest)
    second = i
second
}

def doubleelement(list:List[Int]):List[Int]={

if(list.isEmpty)Nil
else(list.head*2)::doubleelement(list.tail)



} 

def evenfilter(list:List[Int]):List[Int]={
var newlist:List[Int]=Nil
if(list.isEmpty)Nil
else if(list.head%2==0) list.head::evenfilter(list.tail)
else (evenfilter(list.tail))

}

def CountingCharacters(list:String):Int={

if(list.isEmpty)0
else 1+CountingCharacters(list.tail)
}

def reversed (word:String):String={
if(word.isEmpty)""
else reversed(word.tail)+word.head


}
@tailrec
def countelement(list:List[Int],count:Int = 0 ):Int={
if(list== Nil)count
else countelement(list.tail,count+1)
}



def charnum(list:String,c:Char):Int={
  @tailrec
  def helper(list:String,num:Int):Int={
    if(list=="")num

    else if ( list.head == c)
      helper(list.tail,num+1)

    else 
      helper(list.tail,num)


}

helper(list , 0)

}

def calc (list:List[Int],squere:Int= 0):Int={

if(list==Nil)squere

else
    calc(list.tail, squere+(list.head * list.head))


 
}
@tailrec
def issorted(list:List[Int]):Boolean={
    if (list.isEmpty|| list.tail.isEmpty)
       true
    else if(list.head > list.tail.head) 
       false
    else 
       issorted(list.tail)

}

def findindex(list:List[Int],num:Int):Int={
    @tailrec
    def helper(list:List[Int],index:Int):Int={
     if (list.isEmpty) 
      return -1
    else if (list.head == num) 
      return index
    else 
      helper(list.tail, index + 1)
  }
    
helper(list,0)
}



@main 
def run() : Unit ={
val numbers = List(1, 5, 7, 8, 15) 
val char = "mohamed fathy "
val chars = charnum(char,'m')
val lastoddd = findindex(numbers,10)
val bool=issorted(numbers)
println("kl:"+lastoddd)

}