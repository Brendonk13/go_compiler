import java.util.Arrays;

public class Slice<E> {
    private E[] elements = (E[]) new Object[1];
    private int size = 0;

    public Slice<E> add(E item){
        if(this.size == this.elements.length) {
            this.elements = Arrays.copyOf(this.elements, (this.elements.length * 2));
        }
        this.elements[this.size] = item;
        this.size++;
        return this;
    }

    public E get(int index){
        if(index > this.size){
            throw new IllegalArgumentException();
        }
        return this.elements[index];
    }

    public int size(){
        return this.size;
    }

    public int cap(){
        return this.elements.length;
    }
}
