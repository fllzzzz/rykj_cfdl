package com.cf.parking.facade.facade;

public interface ParkingFacade {
    /**
     * 处理僵尸车
     */
    void dealZombieVehicle();

    /**
     * 处理不停车
     */
    void dealNoParking();
}
