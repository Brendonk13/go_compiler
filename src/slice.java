
import java.util.ArrayList;

class Slice {

    int cap;
    int len;
    ArrayList<Object> wrapped;

    public Slice(Object[] vals){
        this.wrapped = new ArrayList<Object>();
        addArrayElems(vals);
        this.cap = this.wrapped.size();
        this.len = this.cap;
    }

    public Slice(){
        this.wrapped = new ArrayList<Object>();
        this.cap = 0;
        this.len = 0;
    }

    // usage go code: x = append(a, 1); --> generated: x.append(a,1);
    public void append(Slice appendTo, Object val){

        if (appendTo.mustReallocate()){
            // allocate new array if appendTo.len == appendTo.cap
            this.wrapped = new ArrayList<Object>(appendTo.wrapped);
            this.wrapped.add(val);
            this.cap = (appendTo.cap == 0) ? 1 : appendTo.cap * 2;
            System.out.println("reallocated in append()");
        }
        else{
            // copy underlying array
            this.wrapped = appendTo.wrapped;
            if (appendTo.wrapped.size() == appendTo.len){
                // must have a value here before trying to replace it with set
                appendTo.wrapped.add(appendTo.len, val);
            }
            else {
                // set replace's the value at appendTo.len
                appendTo.wrapped.set(appendTo.len, val);
            }
            this.cap = appendTo.cap;
        }
        // increment this.len only.
        this.len = appendTo.len + 1;
    }


    public void addAtIndex(int idx, Object val){
        // this function is used whenever a slice idx is assigned to.
        if (mustReallocate()){
            this.wrapped = new ArrayList<Object>(this.wrapped);
            // this.cap *= 2;
            this.cap = (this.cap == 0) ? 1 : this.cap * 2;
            System.out.println("reallocated in addAtIndex()");
        }
        this.wrapped.add(idx, val);
        this.len++;
    }

    public Object getAtIndex(int idx){
        return this.wrapped.get(idx);
    }

    public boolean mustReallocate(){
        // if len == cap we must create a new underlying array and cp all of this obj's elems
        // otherwise we can append the value to this object and return this object.
        return len == cap;
    }

    public void addArrayElems(Object[] vals){
        for (Object val : vals){
            this.wrapped.add(val);
        }
    }



    @Override
    public boolean equals(Object o) { 
        System.out.println("Attempted to use '==' on a slice. This is not valid in Go.");
        System.exit(1);
        return false;
    }


    // testing code

    public void same(){
        // this tests that this slice behaves the same
        // as the 'gotcha' in: https://medium.com/@riteeksrivastava/how-slices-internally-work-in-golang-a47fcb5d42ce
        Slice a = new Slice();
        a.append(a, 1);
        a.append(a, 2);
        Slice b = new Slice();
        b.append(a, 3);
        Slice c = new Slice();
        c.append(a, 4);
        System.out.println("case 1:\na = " + a.getInfo() + "\nb = " + b.getInfo() + "\nc = " + c.getInfo());
        // fmt.Println("a: ", a, "\nb: ", b, "\nc: ", c)
        // right now, a,b,c all point to diff arrays cuz needed to double !!!!!
        //case 2
        System.out.println("");
        a.append(a, 3);
        System.out.println("First append, a = " + a.getInfo());
        System.out.println("");
        Slice x = new Slice();
        x.append(a, 4);
        System.out.println("just did: x.append(a, 4);");
        System.out.println("Second append, a = " + a.getInfo());
        System.out.println("");
        Slice y = new Slice();
        y.append(a, 5);
        System.out.println("just did: y.append(a, 5);");
        System.out.println("Third append, a = " + a.getInfo());
        System.out.println("");
        System.out.println("case 2:\na = " + a.getInfo() + "\nx = " + x.getInfo() + "\ny = " + y.getInfo());
    }


    public void test(){
        this.addAtIndex(0, 0);
        printId("this, idx: 0");
        printInfo();
        System.out.println("");

        this.addAtIndex(1, 1);
        printId("this, idx: 1");
        printInfo();
        System.out.println("");

        this.addAtIndex(2, 2);
        printId("this, idx: 2");
        printInfo();
        System.out.println("");

        this.addAtIndex(3, 3);
        printId("this, idx: 3");
        printInfo();
        System.out.println("");

        Slice ap = new Slice();
        ap.append(this, 777);
        ap.printId("new.append(this)");
        ap.printInfo();
        System.out.println("");
        // printId("after new.append(this)");
        // printInfo();
        printId("state of 'this' after: new.append(this)");
        printInfo();

        System.out.println("pos 2: " + wrapped.get(2));
        System.out.println("pos 3: " + wrapped.get(3));
        System.out.println("pos 4: " + ap.wrapped.get(4));
        // System.out.println("should not work: pos 5:" + wrapped.get(7));
        // addArrayElems(vals);
    }

    public void printId(String id){
        System.out.println("id is: " + id);
    }

    public void printInfo(){
        System.out.println(this.getInfo());
    }

    public String getInfo(){
        return "cap: " + cap + ", len: " + len + "\n  elements: " + wrapped;
    }



    public static void main(String[] args){
        Slice x = new Slice();
        // x.test();
        x.same();
    }
}
