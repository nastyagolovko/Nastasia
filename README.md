# Nastasia
# Метрические алгоритмы
# Алгоритм 1NN
Подбирается метрика. В данной работе это декартово расстояние между векторами.
Обучающая выборка сортируется в порядке увеличения расстояния от классифицируемого элемента.
Элемент относят к тому классу к которому принадлежит ближайший (первый в отсортированной выборке) элемент.
# Алгоритм kNN
Для классификации каждого из объектов тестовой выборки необходимо последовательно выполнить следующие операции:

*Вычислить расстояние до каждого из объектов обучающей выборки

*Отобрать k объектов обучающей выборки, расстояние до которых минимально

*Класс классифицируемого объекта — это класс, наиболее часто встречающийся среди k ближайших соседей
kNN — один из простейших алгоритмов классификации, поэтому на реальных задачах он зачастую оказывается неэффективным. Помимо точности классификации, проблемой этого классификатора является скорость классификации: если в обучающей выборке N объектов, в тестовой выборе M объектов, а размерность пространства — K, то количество операций для классификации тестовой выборки может быть оценено как O(K*M*N). И тем не менее, алгоритм работы kNN является хорошим примером для начала знакомства с Machine Learning.
# Перечислим положительные особенности:
Алгоритм устойчив к аномальным выбросам, так как вероятность попадания такой записи в число k-ближайших соседей мала. Если же это произошло, то влияние на голосование (особенно взвешенное) (при k>2) также, скорее всего, будет незначительным, и, следовательно, малым будет и влияние на итог классификации.
Программная реализация алгоритма относительно проста.
Результат работы алгоритма легко поддаётся интерпретации. Экспертам в различных областях вполне понятна логика работы алгоритма, основанная на нахождении схожих объектов.
Возможность модификации алгоритма, путём использования наиболее подходящих функций сочетания и метрик позволяет подстроить алгоритм под конкретную задачу.
Алгоритм KNN обладает и рядом недостатков. Во-первых, набор данных, используемый для алгоритма, должен быть репрезентативным. Во-вторых, модель нельзя "отделить" от данных: для классификации нового примера нужно использовать все примеры. Эта особенность сильно ограничивает использование алгоритма
