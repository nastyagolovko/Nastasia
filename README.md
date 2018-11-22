
# Метрические алгоритмы
# Алгоритм 1NN
Подбирается метрика. В данной работе это декартово расстояние между векторами.
Обучающая выборка сортируется в порядке увеличения расстояния от классифицируемого элемента.
Элемент относят к тому классу к которому принадлежит ближайший (первый в отсортированной выборке) элемент.

<p><img src= "1NN.jpg">
  
## Преимущества:

* простота реализации;

* интерпретируемость решений;

## Недостатки:

* низкое качество классификации;

* приходится хранить всю выборку целиком.

# Алгоритм kNN
Для классификации каждого из объектов тестовой выборки необходимо последовательно выполнить следующие операции:

* Вычислить расстояние до каждого из объектов обучающей выборки

* Отобрать k объектов обучающей выборки, расстояние до которых минимально

* Класс классифицируемого объекта — это класс, наиболее часто встречающийся среди k ближайших соседей
kNN — один из простейших алгоритмов классификации, поэтому на реальных задачах он зачастую оказывается неэффективным. Помимо точности классификации, проблемой этого классификатора является скорость классификации: если в обучающей выборке N объектов, в тестовой выборе M объектов, а размерность пространства — K, то количество операций для классификации тестовой выборки может быть оценено как O(K*M*N). И тем не менее, алгоритм работы kNN является хорошим примером для начала знакомства с Machine Learning.


  Задаем евклидовое расстояние 
  ``` R
  uclideanDistance <- function(u, v)
   {
   sqrt(sum(u - v)^2)
   }
   ``` 
   
``` R
 Применяем метод kNN 
 kNN <- function(xl, z, k) {    
 
Сортируем выборку согласно классифицируемого объекта     
 orderedXl <- sortObjectsByDist(xl, z)     
 n <- dim(orderedXl)[2] - 1         
 
  Получаем классы первых k соседей    
 classes <- orderedXl[1:k, n + 1] 
```


<p><img src= "photofacefun_com_1539847206.jpg">

Преимущества:

* Программная реализация алгоритма относительно проста.

* Результат работы алгоритма легко поддаётся интерпретации. 

* Возможность модификации алгоритма, путём использования наиболее подходящих функций сочетания и метрик позволяет подстроить алгоритм под конкретную задачу.

Проблемы:

* Модель нельзя "отделить" от данных: для классификации нового примера нужно использовать все примеры. Эта особенность сильно ограничивает использование алгоритма


<p><img src= "loo.png">
  <p><img src= "kwnn.png">

# Сравнение качества алгоритмов kNN и kwNN.
kNN — один из простейших алгоритмов классификации, поэтому на реальных задачах он зачастую оказывается неэффективным. Основными проблемами классификатора  является точность и скорость классификации: если в обучающей выборке N объектов, в тестовой выборе M объектов, а размерность пространства — K, то количество операций для классификации тестовой выборки может быть оценено как O(KMN).
kwNN отличается от kNN, тем что учитывает порядок соседей классифицируемого объекта, улчшая качество классификации.

```R
Применение метоа kNN и сортировка выборки согласно классифицируемого объекта и ррасстояния
KWNN <- function(xl, z, k, q) { 
orderedXl <- sortObjectsByDist(xl, z, euclideanDistance)
 
 
  Метод скользящего контроля для подбора оптимального k
 LOOKWNN <- function(classificator){                    
  j <- 1
  LOO <- seq(1, length(segment))
  
  
  Берем размерност по толбцам и получае класс первых k соседей
  n <- dim(orderedXl)[2] - 1 
  classes <- orderedXl[1:k, n + 1]
  
  
  Реализация весовой функции
   w <- q ^ i                                             
    m[[classes[i]]] <- m[[classes[i]]] + w                  
  }
```


# Метод парзеновского окна 
Метод байесовской классификации, основанный на непараметрическом восстановлении плотности по имеющейся выборке. 
После ввода метрики, метод парзеновского окна можно использовать, не опираясь на вероятностную природу данных. 
## Идея метода 
В основе подхода лежит идея о том, что плотность выше в тех точках, рядом с которыми находится большое количество объектов выборки. 
Если мощность множества элементарных исходов много меньше размера выборки, то в качестве восстановленной по выборке плотности мы вполне можем взять и гистограмму значений выборки. 
В противном случае (например, непрерывном) данный подход не применим, так как плотность концентрируется вблизи обучающих объектов, и функция распределения претерпевает резкие скачки. Приходится использовать восстановление методом Парзена-Розенблатта. 

## Преимущества 
* При классификации объекта заодно оцениваются априорные вероятности его принадлежности каждому из классов. Эта информация используется во многих приложениях для оценки рисков.
* Байесовское решающее правило удобно использовать в качестве эталона при тестировании алгоритмов классификации на модельных данных.
## Недостатки 
* На практике функции правдоподобия классов приходится восстанавливать по конечным выборкам данных. После подстановки восстановленной плотности в формулу байесовский классификатор перестаёт быть оптимальным.
* Методов восстановления плотности известно довольно много. Однако ни один из них не является безусловно лучшим. В практических задачах метод восстановления приходится подбирать экспериментальным путём.


# Метод потенциальных функций  
Метрический классификатор, частный случай метода ближайших соседей. Позволяет с помощью простого алгоритма оценивать вес объектов обучающей выборки при решении задачи классификации. 

В процессе обучения с каждой точкой пространства изображений, соответствующей единичному объекту из обучающей последовательности, связывается функция U(X, Xi), заданная на всем пространстве и зависящая от Xi как от параметра. Такие функции называются потенциальными, так как они напоминают функции потенциала электрического поля вокруг точечного электрического заряда. Изменение потенциала электрического поля по мере удаления от заряда обратно пропорционально квадрату расстояния. Потенциал, таким образом, может служить мерой удаления точки от заряда. Когда поле образовано несколькими зарядами, потенциал в каждой точке этого поля равен сумме потенциалов, создаваемых в этой точке каждым из зарядов. Если заряды, образующие поле, расположены компактной группой, потенциал поля будет иметь наибольшее значение внутри группы зарядов и убывать по мере удаления от нее.

* Достоинства метода потенциальных функций заключаются в нелинейном разбиении множества объектов. Что позволяет решать задачи, которые сложно решить другими методами.
* Недостатки — в трудном выборе подходящей потенциальной функции и трудоемкости вычислений, при большом объеме обучающей выборке.
