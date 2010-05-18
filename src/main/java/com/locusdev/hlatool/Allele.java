package com.locusdev.hlatool;

import java.util.List;

/**
 * User: alexfurman
 * Date: Apr 21, 2010
 * Time: 2:32:05 PM
 */
public class Allele {
    private String name;
    private String sequence;

    public Allele(String name, String sequence) {
        this.name = name;
        this.sequence = sequence;
    }

    public String getName() {
        return name;
    }

    public String getSequence() {
        return sequence;
    }

    public boolean matchesMutation(JMutation mutation) {
        return mutation.matches(getSequence());
    }

    public boolean baseSequenced(Integer input) {
        if (getSequence().length() < (input - 1)) {
            throw new IllegalArgumentException(this + " sequence out of bounds: " + input + ", length: " +
                    getSequence().length());
        }
        return getNucleotide(input) != '*';
    }

    public char getNucleotide(Integer input) {
        return getSequence().charAt(input);
    }

    public boolean matchesMutations(List<JMutation> mutations) {
        for (JMutation mutation : mutations) {
            if (!matchesMutation(mutation)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        return "Allele{" +
                "name='" + name + '\'' +
                '}';
    }
}
